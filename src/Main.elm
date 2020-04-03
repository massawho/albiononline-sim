module Main exposing (..)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List.Extra as L
import Json.Decode as D exposing (Decoder, field, string, int, map2, map3, map5, map6, lazy, index, at)


-- MODELS

type alias CraftResource =
  { count: Int
  , name: String
  }

type alias CraftingRequirement =
  { resources: List CraftResource
  , focus: Int
  , silver: Int
  }

type alias Resource =
  { name: String
  , tier: String
  , enchantmentlevel: Int
  , category: String
  , crafting_requirement: CraftingRequirement
  }

type alias MarketPrice =
  { item_id: String
  , city: String
  , sell_price_min: Int
  , sell_price_max: Int
  , buy_price_min: Int
  , buy_price_max: Int
  }


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL


type State
  = Initial
  | Failure String
  | Fetching
  | Calculating
  | NotFound
  | Success

type alias Setup =
  { return_rate: Float
  , setup_fee: Float
  , tax: Float
  , category_fee: Dict String Int
  }

type alias CustomProduct =
  { quantity: Int
  , product_value: Int
  }

type alias CustomResource =
  { market_price: Int }

type alias CustomInput =
  { products: Dict String CustomProduct
  , resources: Dict String CustomResource
  }

type alias Model =
  { products: List Resource
  , market_price_list: List MarketPrice
  , product_input: String
  , state: State
  , setup: Setup
  , custom_input: CustomInput
  , cities: List String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model
    []
    []
    ""
    Initial
    (Setup 36.5 1.5 3 (Dict.fromList [
      ("leather", 7),
      ("cloth", 35),
      ("metalbar", 53),
      ("planks", 35),
      ("stoneblock", 35)
    ]))
    (CustomInput Dict.empty Dict.empty)
    ["Lymhurst","Bridgewatch","Fort Sterling","Martlock","Thetford"]
  , Cmd.none
  )


-- UPDATE


type Msg
  = AddResource
  | ChangeQuantityInput String String
  | ChangeProductValueInput String String
  | ChangeResourceCostInput String String
  | ChangeResourceInput String
  | ChangeReturnRateInput String
  | ChangeTaxInput String
  | ChangeFeeInput String String
  | ChangeSetupFeeInput String
  | GotResource (Result Http.Error Resource)
  | GotMarketPrices (Result Http.Error (List MarketPrice))

unionNewCustomResourceDict : Resource -> Dict String CustomResource -> Dict String CustomResource
unionNewCustomResourceDict product old_dict =
  List.map (\r -> (r.name, (CustomResource 0))) product.crafting_requirement.resources
  |> Dict.fromList
  |> Dict.union old_dict

newCustomInput : Resource -> CustomInput -> CustomInput
newCustomInput product custom_input =
  { custom_input
    | products = Dict.insert product.name (CustomProduct 1 0) custom_input.products
    , resources = unionNewCustomResourceDict product custom_input.resources
  }

insertFee : String -> String -> Dict String Int -> Dict String Int
insertFee category new_value category_fee_dict =
  Dict.insert category (Maybe.withDefault 0 <| String.toInt new_value) category_fee_dict

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeResourceInput new_value ->
      ({model | product_input = new_value}, Cmd.none)
    ChangeReturnRateInput new_value ->
      let
        setup = model.setup
        new_setup = {setup | return_rate = String.toFloat new_value |> Maybe.withDefault 0}
      in
      ({model | setup = new_setup}, Cmd.none)
    ChangeTaxInput new_value ->
      let
        setup = model.setup
        new_setup = {setup | tax = String.toFloat new_value |> Maybe.withDefault 0}
      in
      ({model | setup = new_setup}, Cmd.none)
    ChangeSetupFeeInput new_value ->
      let
        setup = model.setup
        new_setup = {setup | setup_fee = String.toFloat new_value |> Maybe.withDefault 0}
      in
      ({model | setup = new_setup}, Cmd.none)
    AddResource ->
      ({model | state = Fetching}, getResource model.product_input)
    ChangeFeeInput category new_value ->
      let
        setup = model.setup
        new_setup = {setup | category_fee = insertFee category new_value model.setup.category_fee}
      in
      ({model | setup = new_setup}, Cmd.none)

    ChangeProductValueInput product_name new_value ->
      let
        custom_input = model.custom_input
        new_custom_input =
          { custom_input
            | products = Dict.update product_name
              (\p -> case p of
                Just n ->
                  Just { n | product_value = String.toInt new_value |> Maybe.withDefault 0 }
                Nothing ->
                  Nothing
              )
              model.custom_input.products
            }
      in
      ({ model | custom_input = new_custom_input }, Cmd.none)
    ChangeResourceCostInput resource_name new_value ->
      let
        custom_input = model.custom_input
        new_custom_input =
          { custom_input
            | resources = Dict.update resource_name
              (\p -> case p of
                Just n ->
                  Just { n | market_price = String.toInt new_value |> Maybe.withDefault 0 }
                Nothing ->
                  Nothing
              )
              model.custom_input.resources
            }
      in
      ({ model | custom_input = new_custom_input }, Cmd.none)
    ChangeQuantityInput product_name new_value ->
      let
        custom_input = model.custom_input
        new_custom_input =
          { custom_input
            | products = Dict.update product_name
              (\p -> case p of
                Just n ->
                  Just { n | quantity = String.toInt new_value |> Maybe.withDefault 0 }
                Nothing ->
                  Nothing
              )
              model.custom_input.products
            }
      in
      ({ model | custom_input = new_custom_input }, Cmd.none)

    GotResource result ->
      case result of
        Ok resource ->
          ({model
            | state = Calculating
            , product_input = ""
            , products = resource :: model.products
            , custom_input = (newCustomInput resource model.custom_input)
          }
          , getMarketPrice resource model.cities
          )

        Err (Http.BadBody message) ->
          ({model | state = Failure message}, Cmd.none)

        Err (Http.BadStatus 404) ->
          ({model | state = Failure "Not found"}, Cmd.none)

        Err _ ->
          ({model | state = Failure "Something went wrong"}, Cmd.none)
    GotMarketPrices result ->
      case result of
        Ok market_price_list ->
          ({model
            | state = Success
            , market_price_list = List.append market_price_list model.market_price_list
          }
          , Cmd.none
          )

        Err (Http.BadBody message) ->
          ({model | state = Failure message}, Cmd.none)

        Err _ ->
          ({model | state = Failure "Something went wrong when trying to fetch market data"}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

renderState : State -> Html Msg
renderState state =
  case state of
    Failure message ->
      text message

    Calculating ->
      text "Calculating..."

    Fetching ->
      text "Fetching..."

    _ ->
      div [] []

filterMarketPriceByResourceName : List MarketPrice -> String -> List MarketPrice
filterMarketPriceByResourceName market_price_list resource_name =
  List.filter (\m -> resource_name == m.item_id) market_price_list

renderCategoryFeeInput category category_fee =
  input
    [ Dict.get category category_fee
      |> Maybe.withDefault 0
      |> String.fromInt
      |> value
    , onInput (ChangeFeeInput category)
    ] []

view : Model -> Html Msg
view model =
  div []
    [ input [value model.product_input, onInput ChangeResourceInput] []
    , input [String.fromFloat model.setup.return_rate |> value, onInput ChangeReturnRateInput] []
    , input [String.fromFloat model.setup.setup_fee |> value, onInput ChangeSetupFeeInput] []
    , input [String.fromFloat model.setup.tax |> value, onInput ChangeTaxInput] []
    , renderCategoryFeeInput "leather" model.setup.category_fee
    , renderCategoryFeeInput "cloth" model.setup.category_fee
    , renderCategoryFeeInput "planks" model.setup.category_fee
    , renderCategoryFeeInput "metalbar" model.setup.category_fee
    , renderCategoryFeeInput "stoneblock" model.setup.category_fee
    , button [ onClick AddResource ] [ text "More Please!" ]
    , renderState model.state
    , div []
      ( List.map (
          productTable
            model.custom_input
            model.market_price_list
            model.setup
            model.cities
          ) model.products
      ++ [ renderShoppingList
          model.custom_input
          model.market_price_list
          model.cities
          model.products
        ]
      )
    ]

-- PRODUCT TABLE

renderMarketPriceRow : Float -> Float -> Float -> ProductionCalc -> Html Msg
renderMarketPriceRow return_rate setup_fee tax production_calc =
  tr [] [ td [ attribute "colspan" "4" ] []
    , td [] [ text production_calc.description ]
    , td [] [ text (String.fromInt production_calc.total_production_cost) ]
    , td [] [ text (String.fromInt (calcReturnRate return_rate production_calc.total_production_cost)) ]
    , td [] [ text (String.fromInt production_calc.product_value.sell_price_min) ]
    , td [] [ text (String.fromInt 0) ]
    , td [] [ (calcInvestment production_calc.total_production_cost production_calc.total_fee) * 1 |> String.fromInt |> text ]
    , td [] [ calcProfit production_calc return_rate setup_fee tax 1 |> String.fromInt |> text ]
    , td [] [ (calcInvestment production_calc.total_production_cost production_calc.total_fee) * 100 |> String.fromInt |> text ]
    , td [] [ calcProfit production_calc return_rate setup_fee tax 100 |> String.fromInt |> text ]
    , td [] [ (calcInvestment production_calc.total_production_cost production_calc.total_fee) * 1000 |> String.fromInt |> text ]
    , td [] [ calcProfit production_calc return_rate setup_fee tax 1000 |> String.fromInt |> text ]
    ]

renderIttemImg : String -> Html a
renderIttemImg name =
  img [src ("https://gameinfo.albiononline.com/api/gameinfo/items/" ++ name ++ ".png")] []

renderProductRow : CustomInput -> Setup -> Resource -> Html Msg
renderProductRow custom_input setup product =
  case Dict.get product.name custom_input.products of
    Just custom_product ->
      let
        custom_calc = calcCustomProduction setup.category_fee custom_input product
      in
      tr []
        [ td [] [ renderIttemImg product.name ]
        , td [] [ text product.name ]
        , td [] [ text product.tier ]
        , td [] [ input [ type_ "text", value <| String.fromInt custom_product.quantity, onInput (ChangeQuantityInput product.name) ] [] ]
        , td [] [ text "Custom" ]
        , td [] [ text (String.fromInt custom_calc.total_production_cost) ]
        , td [] [ text (String.fromInt (calcReturnRate setup.return_rate custom_calc.total_production_cost)) ]
        , td [] [ input [ type_ "text", value <| String.fromInt custom_product.product_value, onInput (ChangeProductValueInput product.name) ] [] ]
        , td [] [ text "0" ]
        , td [] [ (calcInvestment custom_calc.total_production_cost custom_calc.total_fee) * 1 |> String.fromInt |> text ]
        , td [] [ calcProfit custom_calc setup.return_rate setup.setup_fee setup.tax 1 |> String.fromInt |> text ]
        , td [] [ (calcInvestment custom_calc.total_production_cost custom_calc.total_fee) * 100 |> String.fromInt |> text ]
        , td [] [ calcProfit custom_calc setup.return_rate setup.setup_fee setup.tax 100 |> String.fromInt |> text ]
        , td [] [ (calcInvestment custom_calc.total_production_cost custom_calc.total_fee) * 1000 |> String.fromInt |> text ]
        , td [] [ calcProfit custom_calc setup.return_rate setup.setup_fee setup.tax 1000 |> String.fromInt |> text ]
        ]
    Nothing ->
      tr [] []

productTable : CustomInput -> List MarketPrice -> Setup -> List String -> Resource -> Html Msg
productTable custom_input market_price_list setup cities product =
  table []
    ([ tr []
      [ td [ attribute "colspan" "3" ] [ text " " ]
      , td [ attribute "colspan" "6" ] [ text " " ]
      ]
    , tr []
      [ th [] [ text " " ]
      , th [] [ text "Name" ]
      , th [] [ text "Tier" ]
      , th [] [ text "Quantity" ]
      , th [] [ text " " ]
      , th [] [ text "Input cost" ]
      , th [] [ text "Cost after return" ]
      , th [] [ text "Product value" ]
      , th [] [ text "Profit/quantity" ]
      , th [] [ text "Investment/1" ]
      , th [] [ text "Profit/1" ]
      , th [] [ text "Investment/100" ]
      , th [] [ text "Profit/100" ]
      , th [] [ text "Investment/1000" ]
      , th [] [ text "Profit/1000" ]
      ]
    , renderProductRow custom_input setup product
    ] ++ (calcProductionTable setup.category_fee market_price_list product cities
          |> List.map (renderMarketPriceRow setup.return_rate setup.setup_fee setup.tax))
    )

-- SHOPPING LIST

renderShoppingListPrices market_price_list resource_name city_name =
  case getMarketPriceByResourceAndCity market_price_list city_name resource_name of
    Just market_price ->
      td [] [ text (String.fromInt market_price.sell_price_min) ]
    Nothing ->
      td [] []

getCustomProductPrice : Dict String CustomProduct -> String -> MarketPrice
getCustomProductPrice custom_product_dict resource_name =
  case Dict.get resource_name custom_product_dict of
    Just product ->
      MarketPrice resource_name "custom" product.product_value 0 0 0
    Nothing ->
      MarketPrice resource_name "custom" 0 0 0 0

getCustomResourcePrice : Dict String CustomResource -> String -> MarketPrice
getCustomResourcePrice custom_resource_dict resource_name =
  case Dict.get resource_name custom_resource_dict of
    Just resource ->
      MarketPrice resource_name "custom" resource.market_price 0 0 0
    Nothing ->
      MarketPrice resource_name "custom" 0 0 0 0

renderShoppingListRow : CustomInput -> List MarketPrice -> List String -> CraftResource -> Html Msg
renderShoppingListRow custom_input market_price_list cities craft_resource =
  tr []
    (
    [ td [] [ renderIttemImg craft_resource.name ]
    , td [] [ text craft_resource.name ]
    , td [] [ text (String.fromInt craft_resource.count) ]
    , td [] [ text (String.fromInt craft_resource.count) ]
    , td []
      [ input
        [ onInput (ChangeResourceCostInput craft_resource.name)
        , value (String.fromInt((getCustomResourcePrice custom_input.resources craft_resource.name).sell_price_min))
        ]
      [] ]
    ]
    ++ List.map (renderShoppingListPrices market_price_list craft_resource.name) cities)

renderShoppingList : CustomInput -> List MarketPrice -> List String -> List Resource -> Html Msg
renderShoppingList custom_input market_price_list cities products =
  table []
    ([ tr []
      ([ th [] [ text " " ]
      , th [] [ text "Resource" ]
      , th [] [ text "Requirement" ]
      , th [] [ text "Σ Required" ]
      , th [] [ text "Custom cost" ]
      ] ++ List.map (\c -> th [] [ text c ]) cities)
    ] ++ List.map (renderShoppingListRow custom_input market_price_list cities) (List.concatMap (getCraftingList) products)
    )


-- HTTP


getCraftingList : Resource -> List CraftResource
getCraftingList resource =
  List.map (\r -> r) resource.crafting_requirement.resources

generateResourceList : List String -> String
generateResourceList list =
  List.foldl (++) "" (List.intersperse "," list)

generateAodLink : Resource -> List String -> String
generateAodLink resource cities =
  "https://www.albion-online-data.com/api/v1/stats/prices/"
  ++ generateResourceList(resource.name :: (getCraftingList resource |> List.map (\r -> r.name)))
  ++ "?locations="
  ++ (generateResourceList cities)

getMarketPrice : Resource -> List String -> Cmd Msg
getMarketPrice resource cities =
  Http.get
    { url = generateAodLink resource cities
    , expect = Http.expectJson GotMarketPrices marketPriceDecoder
    }


getResource: String -> Cmd Msg
getResource resource_unique_name =
  Http.get
    { url = "http://localhost:4000/api/resources/" ++ resource_unique_name,
      expect = Http.expectJson GotResource resourceDecoder
    }

stringToInt value =
  case String.toInt value of
    Just number -> D.succeed number
    Nothing -> D.fail "not a number"

maybeStringToInt : Maybe String -> Decoder Int
maybeStringToInt value =
  case String.toInt (Maybe.withDefault "0" value) of
    Just number -> D.succeed number
    _ -> D.fail "not a number"

craftResourceDecoder : Decoder CraftResource
craftResourceDecoder =
  map2 CraftResource
    (field "@count" string
      |> D.andThen stringToInt)
    (field "@uniquename" string)

craftingRequirementDecoder : Decoder CraftingRequirement
craftingRequirementDecoder =
  D.map3 CraftingRequirement
    (field "craftresource" (D.list craftResourceDecoder))
    (field "@craftingfocus" string
      |> D.andThen stringToInt)
    (field "@silver" string
      |> D.andThen stringToInt)

resourceDecoder : Decoder Resource
resourceDecoder =
  D.map5 Resource
    (field "data" (field "unique_name" string))
    (D.at ["data", "meta", "@tier"] string)
    (D.maybe (D.at ["data", "meta", "@enchantmentlevel"] string)
      |> D.andThen maybeStringToInt)
    (D.at ["data", "meta", "@shopsubcategory1"] string)
    (at ["data", "meta"] (field "craftingrequirements" (index 0 craftingRequirementDecoder)))

marketPriceDecoderOne : Decoder MarketPrice
marketPriceDecoderOne =
  map6 MarketPrice
    (field "item_id" string)
    (field "city" string)
    (field "sell_price_min" int)
    (field "sell_price_max" int)
    (field "buy_price_min" int)
    (field "buy_price_max" int)

marketPriceDecoder : Decoder (List MarketPrice)
marketPriceDecoder =
  D.list marketPriceDecoderOne


-- PRODUCT TABLE
getMarketPriceByResourceAndCity : List MarketPrice -> String -> String -> Maybe(MarketPrice)
getMarketPriceByResourceAndCity market_price_list city_name resource_name =
  List.filter (\m -> m.city == city_name && m.item_id == resource_name) market_price_list
  |> List.head


type alias CraftingCost =
  { craft_resource: CraftResource
  , market_price: MarketPrice
  , total: Int
  }

type alias ProductionCalc =
  { description: String
  , product_value: MarketPrice
  , resources_costs: List CraftingCost
  , total_fee: Int
  , total_production_cost: Int
  }

calcCraftingCost : (CraftResource, MarketPrice) -> CraftingCost
calcCraftingCost (craft_resource, market_price) =
  { craft_resource = craft_resource
  , market_price = market_price
  , total = craft_resource.count * market_price.sell_price_min
  }

calcProductionCalc : Int -> List CraftingCost -> Int
calcProductionCalc silver crafting_cost_list =
  crafting_cost_list
  |> List.map (\craft_cost -> craft_cost.total)
  |> List.foldl (+) silver

calcReturnRate : Float -> Int -> Int
calcReturnRate return_rate craft_cost =
  craft_cost
  |> toFloat
  |> (*)(1 - return_rate/100)
  |> round


calcCustomProduction : Dict String Int -> CustomInput -> Resource -> ProductionCalc
calcCustomProduction category_fee_dict custom_input product =
  let
    crafting_cost_list =
      List.map (\craft_resource -> craft_resource.name) product.crafting_requirement.resources
      |> List.map (getCustomResourcePrice custom_input.resources)
      |> List.map2 Tuple.pair product.crafting_requirement.resources
      |> List.map calcCraftingCost
  in
  { description = "Custom"
  , product_value = getCustomProductPrice custom_input.products product.name
  , resources_costs = crafting_cost_list
  , total_fee = calcFee category_fee_dict product
  , total_production_cost = calcProductionCalc
      product.crafting_requirement.silver
      crafting_cost_list
  }


calcTotalCraftingCost : List MarketPrice -> CraftingRequirement -> String -> List CraftingCost
calcTotalCraftingCost market_price_list crafting_requirement city_name =
  List.map (\craft_resource -> craft_resource.name) crafting_requirement.resources
  |> List.filterMap (getMarketPriceByResourceAndCity market_price_list city_name)
  |> List.map2 Tuple.pair crafting_requirement.resources
  |> List.map calcCraftingCost

fetchProductValue : Resource -> Int
fetchProductValue product =
  let
    product_value_list = Dict.fromList [((2, 0), 2),
      ((3, 0), 6), ((4, 0), 14), ((5, 0), 30), ((6, 0), 62), ((7, 0), 126), ((8, 0), 254),
      ((4, 1), 30), ((5, 1), 62), ((6, 1), 126), ((7, 1), 245), ((8, 1), 512),
      ((4, 2), 54), ((5, 2), 118), ((6, 2), 246), ((7, 2), 502), ((8, 2), 1000),
      ((4, 3), 102), ((5, 3), 230), ((6, 3), 486), ((7, 3), 998), ((8, 3), 2000)]
    tier = product.tier
      |> String.toInt
      |> Maybe.withDefault 0
  in
    case Dict.get (tier, product.enchantmentlevel) product_value_list of
      Just product_value -> product_value
      _ -> 0

fetchCategoryFee : Dict String Int -> Resource -> Int
fetchCategoryFee category_fee_dict product =
  case Dict.get product.category category_fee_dict of
    Just fee -> fee
    _ -> 0

calcFee : Dict String Int -> Resource -> Int
calcFee category_fee_dict product =
  round
    ( toFloat((fetchCategoryFee category_fee_dict product) * (fetchProductValue product) * 5) / 100 )

findCheapestResource : List MarketPrice -> CraftResource -> Maybe MarketPrice
findCheapestResource market_price_list resource =
  market_price_list
  |> List.filter (\market_price -> market_price.item_id == resource.name)
  |> L.minimumWith (\x y -> compare x.sell_price_min y.sell_price_min)

calcOptmisedCraftingCost market_price_list product =
  List.filterMap (findCheapestResource market_price_list) product.crafting_requirement.resources
  |> List.map2 Tuple.pair product.crafting_requirement.resources
  |> List.map calcCraftingCost

calcOptmisedProduction : Dict String Int -> List MarketPrice -> Resource -> List ProductionCalc -> Maybe ProductionCalc
calcOptmisedProduction category_fee_dict market_price_list product production_calc_list =
  case getBestSell production_calc_list of
    Just best_sell ->
      let
        crafting_cost_list = calcOptmisedCraftingCost
          market_price_list
          product
      in
      Just({ description = "Optmised"
      , product_value = best_sell.product_value
      , resources_costs = crafting_cost_list
      , total_fee = calcFee category_fee_dict product
      , total_production_cost = calcProductionCalc
          product.crafting_requirement.silver
          crafting_cost_list
      })
    Nothing ->
      Nothing

calcProductionByCity : Dict String Int -> List MarketPrice -> Resource -> String -> Maybe ProductionCalc
calcProductionByCity category_fee_dict market_price_list product city_name =
  case getMarketPriceByResourceAndCity market_price_list city_name product.name of
    Just market_price ->
      let
        crafting_cost_list = calcTotalCraftingCost
          market_price_list
          product.crafting_requirement
          city_name
      in
      Just({ description = city_name
      , product_value = market_price
      , resources_costs = crafting_cost_list
      , total_fee = calcFee category_fee_dict product
      , total_production_cost = calcProductionCalc
          product.crafting_requirement.silver
          crafting_cost_list
      })
    Nothing ->
      Nothing

generateDescription description b s =
  description
  ++ " ("
  ++ b.product_value.city
  ++ " - "
  ++ (s.resources_costs |> List.map(\r -> r.market_price.city) |> L.unique |> generateResourceList)
  ++ ")"

getBestBuy : List ProductionCalc -> Maybe ProductionCalc
getBestBuy production_calc_list =
  production_calc_list
    |> L.minimumWith (\x y -> compare x.total_production_cost y.total_production_cost)

getBestSell : List ProductionCalc -> Maybe ProductionCalc
getBestSell production_calc_list =
  production_calc_list
      |> L.maximumWith (\x y -> compare x.product_value.sell_price_min y.product_value.sell_price_min)

getWorstSell : List ProductionCalc -> Maybe ProductionCalc
getWorstSell production_calc_list =
  production_calc_list
      |> L.minimumWith (\x y -> compare x.product_value.sell_price_min y.product_value.sell_price_min)

calcProductionByMinMax : List ProductionCalc -> Maybe ProductionCalc
calcProductionByMinMax production_calc_list  =
  let
    best_buy = getBestBuy production_calc_list
    best_sell = getBestSell production_calc_list
  in
  case (best_buy, best_sell) of
    (Just b, Just s) ->
      Just(ProductionCalc (generateDescription "Min - Max" b s)
        s.product_value
        b.resources_costs
        b.total_fee
        b.total_production_cost
      )
    _ ->
      Nothing

calcProductionByMinMin : List ProductionCalc -> Maybe ProductionCalc
calcProductionByMinMin production_calc_list  =
  let
    best_buy = getBestBuy production_calc_list

    worst_sell = getWorstSell production_calc_list
  in
  case (best_buy, worst_sell) of
    (Just b, Just s) ->
      Just(ProductionCalc (generateDescription "Min - Min" b s)
        s.product_value
        b.resources_costs
        b.total_fee
        b.total_production_cost
      )
    _ ->
      Nothing


appendCalcIf fn list final_list =
  case fn list of
    Just v ->
      v :: final_list
    Nothing ->
      final_list

calcProductionTable : Dict String Int -> List MarketPrice -> Resource -> List String -> List ProductionCalc
calcProductionTable category_fee_dict market_price_list product cities =
  let
    calc_list = List.filterMap (calcProductionByCity category_fee_dict market_price_list product) cities
  in
  appendCalcIf (calcProductionByMinMax) calc_list calc_list
  |> appendCalcIf (calcProductionByMinMin) calc_list
  |> appendCalcIf (calcOptmisedProduction category_fee_dict market_price_list product) calc_list


calcProfit : ProductionCalc -> Float -> Float -> Float -> Int -> Int
calcProfit production_calc return_rate setup_fee tax craft_amount =
  round((toFloat(production_calc.product_value.sell_price_min) * (1-(setup_fee+tax)/100)) - toFloat(calcInvestment (calcReturnRate return_rate production_calc.total_production_cost) production_calc.total_fee)) * craft_amount

calcInvestment : Int -> Int -> Int
calcInvestment total_production_cost total_fee =
  total_production_cost + total_fee

module Models.Resource exposing (..)

type CraftingRequirement =
  CraftingRequirement
    { resource: List Resource,
      count: Int
    }

type Resource =
  Resource
    { name: String,
      tier: String,
      crafting_requirement: List CraftingRequirement
    }

type alias MarketPrice =
  { item_id: String
  , city: String
  , sell_price_min: Int
  , sell_price_max: Int
  , buy_price_min: Int
  , buy_price_max: Int
  }

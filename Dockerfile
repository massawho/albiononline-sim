FROM codesimple/elm:0.19

WORKDIR /app

ADD package.json package.json
ADD yarn.lock yarn.lock

RUN apk add yarn && \
    yarn install

ADD . .

CMD ["yarn", "start"]

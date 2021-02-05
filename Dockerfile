FROM ubuntu:18.04 AS BUILD_IMAGE
 
WORKDIR /app

RUN apt update && apt -y install curl dirmngr apt-transport-https lsb-release ca-certificates && curl -sL https://deb.nodesource.com/setup_10.x | bash && apt-get install -y nodejs && apt install -y netbase && apt install -y git && rm -rf /var/lib/apt/lists/*

ADD https://github.com/coot/zephyr/releases/download/v0.3.2/Linux.tar.gz /tmp/zephyr/

RUN tar -xzf /tmp/zephyr/Linux.tar.gz

ENV PATH="/app/zephyr:${PATH}"
 
COPY package.json package.json
COPY package-lock.json package-lock.json
 
RUN npm install

COPY . .

RUN npm run build  && rm -rf output && rm -rf dce-output

RUN npm prune --production

FROM node:10-slim

WORKDIR /app

COPY --from=BUILD_IMAGE /app/package.json ./package.json
COPY --from=BUILD_IMAGE /app/dist ./dist
COPY --from=BUILD_IMAGE /app/node_modules ./node_modules
 
CMD [ "npm", "start" ]

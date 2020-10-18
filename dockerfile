FROM node:8.11.4-alpine

RUN mkdir -p /src
RUN npm install express -g

RUN pwd
WORKDIR /src/
ADD ./package.json /src/package.json
ADD ./build /src/build
ADD ./server.js /src/server.js
RUN npm install

EXPOSE 3000
CMD npm start
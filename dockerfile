FROM node:8.11.4-alpine

ENV API_KEY=CVQNCvZfIk9YWo4TkAK6KopdZyHo1DoXrjvDhl7X4yA
ENV PORT=80

RUN mkdir -p /src && \
   npm install express -g

WORKDIR /src/
ADD ./package.json /src/package.json
ADD ./build /src/build
ADD ./routes /src/routes
ADD ./server.js /src/server.js
RUN npm install

CMD npm run prod
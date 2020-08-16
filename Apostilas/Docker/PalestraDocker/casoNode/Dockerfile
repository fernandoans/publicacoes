FROM node:6.5.0

WORKDIR /app

RUN npm install nodemon -g

COPY index.js /app/index.js
COPY package.json /app/package.json
RUN npm install

EXPOSE 3000

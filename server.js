const express = require('express');
const path = require('path');
const uuid = require('uuid');
const fetch = require('node-fetch');
const { query } = require('express');
const unsplashKey = 'CVQNCvZfIk9YWo4TkAK6KopdZyHo1DoXrjvDhl7X4yA';
const unsplashEndpoint = 'https://api.unsplash.com/photos/';

console.log(`server root set to: \n${path.join(__dirname + '/build')}`);
const port = 3000;
const app = express();

app.use(logger);

app.use('/static', express.static('./build/static'));
app.get('/api/unsplash ', unsplashApi);
app.get('/api', notFoundApi);

// SPA
app.get('/*', (req, res) => {
  console.log(`[${req.uuid}] -SPA- `);
  res.sendFile(path.join(__dirname + '/build/index.html'));
});
app.all('*', (req, res) => {
  console.log(`[${req.uuid}] -catch- `);

  res.status(405);
  res.set({ Allow: 'GET' });
  res.send('<p>Method Not Supported</p>');
});

// LOGGER
function logger(req, res, next) {
  req.uuid = uuid.v4();
  // console.log(`${req.method} to ${req.url}`);
  console.log(`[${req.uuid}] ${req.method} to ${req.url}`);
  next();
}

// INIT
app.listen(port, () => {
  console.log('listening to port 3000');
});

// API
// TODO: consider passing request along bvewtween unsplash
// and frntend whilst just adding the auth header
function unsplashApi(req, res) {
  console.log(`[${req.uuid}] -API- ${req.params.endpoint}`);
  let url = unsplashEndpoint + req.params.endpoint;
  if (req.query) {
    url += `?`;
    for (const key in req.query) {
      url += `${key}=${req.query[key]}&`;
    }
    url = url.slice(0, -1);
    console.log(req.params);
  }
  fetch(url, { method: 'get' })
    .then(raw => raw)
    .then(json => res.json(json))
    .catch(err =>
      console.log(`[${req.uuid}] - api-passthrough failed. ${err}`)
    );
}
function notFoundApi(req, res) {
  res
    .status(404)
    .json({ status: 404, messege: 'This route is not in use', url: req.url });
}

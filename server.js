const express = require('express');
const path = require('path');
const uuid = require('uuid');
const bodyParser = require('body-parser');
const { unsplashApi, apiNotFound, jwtGet, jwtTest } = require('./routes');

console.log(`server root set to: \n${path.join(__dirname + '/build')}`);
const port = 3000;
const app = express();

app.use(logger);
app.use(bodyParser.json());

app.use('/static', express.static('./build/static'));
app.get('/api/unsplash/*', unsplashApi);
app.get('/jwt/get', jwtGet);
app.get('/jwt/test', jwtTest);
app.get('/api*+', apiNotFound);

// SPA
app.get('/*', (req, res) => {
  console.log(`[${req.uuid}] -SPA redirect- `);
  res.sendFile(path.join(__dirname + '/build/index.html'));
});

app.all('*', (req, res) => {
  console.log(`[${req.uuid}] -catch all route- `);
  res.status(405);
  res.set({ Allow: 'GET' });
  res.send('<p>Method Not Supported</p>');
});

// LOGGER
function logger(req, res, next) {
  req.uuid = uuid.v4();
  console.log(`[${req.uuid}] ${req.method} to ${req.url}`);
  next();
}

// INIT
app.listen(port, () => {
  console.log('listening to port 3000');
});

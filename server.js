const express = require('express');
const path = require('path');
const uuid = require('uuid');
const fetch = require('node-fetch');
global.fetch = fetch;
const Unsplash = require('unsplash-js').default;
const toJson = require('unsplash-js').toJson;

const unsplash = new Unsplash({
  accessKey: 'CVQNCvZfIk9YWo4TkAK6KopdZyHo1DoXrjvDhl7X4yA',
  timeout: 500, // values set in ms
});

console.log(`server root set to: \n${path.join(__dirname + '/build')}`);
const port = 3000;
const app = express();

app.use(logger);

app.use('/static', express.static('./build/static'));
app.get('/api/random', randomHandler);
app.get('/api/search', search);

// SPA
app.get('/*', (req, res, next) => {
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
function randomHandler(req, res) {
  unsplash.photos
    .getRandomPhoto({ featured: true })
    .then(toJson)
    .then(json => {
      if (json.errors) {
        throw json;
      } else {
        console.log(`[${req.uuid}] -API- sent picture [${json.id}]`);
        const imageJson = {
          id: json.id,
          desc: json.description ? json.description : 'no desc',
          alt: json.alt_description ? json.alt_description : 'no alt availible',
          url: json.urls.regular,
          likes: json.likes,
          user: {
            name: json.user.name,
            location: json.user.location,
            bio: json.user.bio ? json.user.bio : 'no bio availible',
          },
        };
        res.json(imageJson);
      }
    })
    .catch(err => {
      console.log(`[${req.uuid}] -API- unsplash error [${err}]`);
      res.status = 500;
      res.json({
        uuid: req.uuid,
        error: JSON.stringify(err),
        err: err.errors,
      });
    });
}
function search(req, res) {
  unsplash.search
    .photos(req.query.q, 1, 10)
    .then(toJson)
    .then(json => {
      const response = {
        total: json.total,
        totalPages: json.total_pages,
        results: json.results.map(image => {
          return {
            id: image.id,
            desc: image.description ? image.description : 'no desc',
            alt: image.alt_description
              ? image.alt_description
              : 'no alt availible',
            url: image.urls.regular,
            likes: image.likes,
            user: {
              name: image.user.name,
              location: image.user.location,
              bio: image.user.bio ? image.user.bio : 'no bio availible',
            },
          };
        }),
      };
      res.json(response);
    });
}

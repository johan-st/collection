const unsplashKey = process.env.API_KEY;
const unsplashEndpoint = 'https://api.unsplash.com/';
const fetch = require('node-fetch');

// TODO: consider passing request along bvewtween unsplash
// and frntend whilst just adding the auth header

function unsplashApi(req, res, next) {
  console.log(`[${req.uuid}] -API- `);
  let url = unsplashEndpoint + req.params[0];
  if (req.query) {
    url += `?`;
    for (const key in req.query) {
      url += `${key}=${req.query[key]}&`;
    }
    url = url.slice(0, -1);
    console.log(req.params);
    console.log(url.split('unsplash/'));
  }
  console.log(`[${req.uuid}] -API PASSTHROUGH-  ${url}`);
  fetch(url, {
    method: 'get',
    headers: {
      Authorization: 'Client-ID ' + unsplashKey,
    },
  })
    .then(raw => raw.json())
    .then(json => {
      // console.log(json);
      return json;
    })
    .then(json => res.json(json))
    .catch(err =>
      console.log(`[${req.uuid}] -API PASSTHROUGH- FAILED. ${err}`)
    );
}
module.exports = unsplashApi;

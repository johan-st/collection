const unsplashApi = require('./unsplash');
const apiNotFound = require('./apiNotFound');
const { jwtGet, jwtTest } = require('./jwt.js');
module.exports = { unsplashApi, apiNotFound, jwtGet, jwtTest };

const unsplashApi = require('./unsplash');
const apiNotFound = require('./apiNotFound');
const { jwtGet, jwtTest } = require('./jwt.js');
const loginRouter = require('./loginRouter');
module.exports = { unsplashApi, apiNotFound, jwtGet, jwtTest, loginRouter };

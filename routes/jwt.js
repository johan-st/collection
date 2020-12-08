const jwt = require('jsonwebtoken');
const prevSecret = 'sten';
const secret = 'sten';

const jwtGet = (req, res, next) => {
  var token = jwt.sign({ msg: 'test' }, secret, {
    algorithm: 'HS512',
  });
  console.log(token);
  res.json({ token, secret });
};

const jwtTest = (req, res, next) => {
  const token = jwt.verify(req.body.token, secret);
  res.json({ token });
};

module.exports = { jwtGet, jwtTest };

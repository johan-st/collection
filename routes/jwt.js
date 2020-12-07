const jwt = require('jsonwebtoken');

const jwtGet = (req, res, next) => {
  var token = jwt.sign(
    { foo: 'bar', iat: Math.floor(Date.now() / 1000) - 30 },
    'shhhhh'
  );
  res.json({ token });
};

const jwtTest = (req, res, next) => {
  const token = jwt.verify(req.body.token, 'shhhhh');
  res.json({ token });
};

module.exports = { jwtGet, jwtTest };

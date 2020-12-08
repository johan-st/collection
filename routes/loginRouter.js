const jwt = require('jsonwebtoken');
const express = require('express');
const router = express.Router();
const secret = 'sten';

router.post('/', (req, res) => {
  const token = res.json({ token, secret });
});

module.exports = router;

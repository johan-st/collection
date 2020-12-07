function apiNotFound(req, res) {
  console.log(`[${req.uuid}] -API_Not_Found-`);
  res
    .status(404)
    .json({ status: 404, messege: 'This route is not in use', url: req.url });
}

module.exports = apiNotFound;

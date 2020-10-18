const express = require("express");
const path = require("path");
const uuid = require("uuid");

console.log(path.join(__dirname + "/build"));
const port = 3000;
const app = express();

app.use(logger);

// STATIC FILES
app.get("/static/*", (req, res, next) => {
  console.log("static: ", req.path);
  res.sendFile(path.join(req.path), {
    root: path.join(__dirname, "build"),
  });
});

// SPA
app.get("/*", (req, res, next) => {
  res.sendFile(path.join(__dirname + "/build/index.html"));
});
app.all("*", (req, res) => {
  res.status(405);
  res.set({ Allow: "GET" });
  res.send("<p>Method Not Supported</p>");
});

// LOGGER
function logger(req, res, next) {
  req.uuid = uuid.v4();
  console.log(`${req.method} to ${req.url}`);
  // console.log(`[${req.uuid}] ${req.method} to ${req.url}`);
  next();
}

// INIT
app.listen(port, () => {
  console.log("listening to port 3000");
});

import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";
import { initSound } from "./sound";

// Extract the stored data from previous sessions.
var storedData = localStorage.getItem("elm-model");
var flags = storedData ? JSON.parse(storedData) : "null";
const sounds = initSound();
const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags,
});

app.ports.log.subscribe((log) => console.log("elm-log-port:\n", log));

// // Listen for commands from the `setStorage` port.
// // Turn the data to a string and put it in localStorage.
app.ports.setPersist.subscribe((persist) => {
  console.log("setPersist: ", persist);
  localStorage.setItem("elm-model", JSON.stringify(persist));
});
// playSound
app.ports.sound.subscribe((select) => {
  if (select === "click") {
    sounds.click.play();
  } else if (select === "coin") {
    sounds.coin.play();
  } else if (select === "keepAlive") {
    sounds.keepAlive.play();
  } else if (select === "jingle") {
    sounds.jingle.play();
  } else if (select === "correct") {
    sounds.correct.play();
  } else {
    console.log("bad sound selection:", select);
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

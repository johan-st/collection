import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

// Extract the stored data from previous sessions.
var storedData = localStorage.getItem('elm-model');
var flags = storedData ? JSON.parse(storedData) : "null";

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags
});

app.ports.log.subscribe(log => console.log(log));

// // Listen for commands from the `setStorage` port.
// // Turn the data to a string and put it in localStorage.
app.ports.setPersist.subscribe(persist => {
  console.log('setPersist: ', persist)
  localStorage.setItem('elm-model', JSON.stringify(persist));
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

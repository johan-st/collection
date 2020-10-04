export function initSound() {
  return {
    click: new Audio("./click.wav"),
    coin: new Audio("./coin.wav"),
    correct: new Audio("./correct8bit.wav"),
    jingle: new Audio("./jingle.wav"),
    keepAlive: new Audio("./silence.wav"),
  };
}

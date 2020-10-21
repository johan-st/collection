export function initSound() {
  return {
    click: new Audio("/static/click.wav"),
    coin: new Audio("/static/coin.wav"),
    correct: new Audio("/static/correct8bit.wav"),
    jingle: new Audio("/static/jingle.wav"),
  };
}

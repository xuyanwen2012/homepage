import './main.css';
import {Elm} from './Main.elm';
import * as serviceWorker from './serviceWorker';

let app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: Pasta.version
});

app.ports.setFilters.subscribe((options) => {
  requestAnimationFrame(() =>
    Pasta.apply(document.getElementById("main-canvas"), options)
  )
});

Pasta.addActivityListener((activity) => {
  console.log("got some activity to send to elm: ", activity);
  app.ports.activityChanges.send(activity);
})

class RangeSlider extends HTMLElement {
  constructor() {
    super();
    this.val = null;
  }

  connectedCallback() {
    const input = document.createElement('input');
    this.appendChild(input);

    const jsr = new JSR(input, {
      max: this.max,
      values: [this.val],
      sliders: 1,
      grid: false
    });

    jsr.addEventListener('update', (elem, value) => {
      const event = new CustomEvent("slide", {
        detail: {userSlidTo: value}
      });

      this.dispatchEvent(event)
    })
  }
}

window.customElements.define("range-slider", RangeSlider);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

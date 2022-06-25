import "./style.css";
import { Elm } from "./Main.elm";

const root = document.querySelector("#app");
const app = Elm.Main.init({ node: root });

app.ports.vibrate.subscribe(() => {
  navigator.vibrate(100);
});

import "./style.css";
import { Elm } from "./Main.elm";

const root = document.querySelector("#app");
const app = Elm.Main.init({ node: root });

app.ports.vibrate.subscribe(() => {
  navigator.vibrate(100);
});

app.ports.fadeStartModalOut.subscribe(() => {
  const modal = document.querySelector("#modal");
  const backdrop = document.querySelector("#backdrop");

  modal
    .animate([{ opacity: 1 }, { opacity: 0 }], {
      duration: 1000,
      easing: "ease-in",
    })
    .addEventListener("finish", () => {
      modal.style.display = "none";
    });

  backdrop
    .animate([{ opacity: 1 }, { opacity: 0 }], {
      duration: 1000,
      easing: "ease-in",
    })
    .addEventListener("finish", () => {
      backdrop.style.display = "none";
    });
});

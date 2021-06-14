import { Elm } from "./src/Main.elm";

const app = Elm.Main.init({
  node: document.querySelector("main")
});

document.addEventListener("scroll", () => {
  app.ports.scrollTop.send(document.documentElement.scrollTop);
});

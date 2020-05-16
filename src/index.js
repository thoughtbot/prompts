import '@fortawesome/fontawesome-free/css/all.css';
import 'normalize.css';
import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

window.addEventListener('DOMContentLoaded', (_) => {
  const storageBackend = process.env.ELM_APP_ENABLE_LOCAL_STORAGE === "true" ? window.localStorage : NullStorage;
  const storage = new AppStorage(storageBackend);

  const elmApp = Elm.Main.init({
    node: document.getElementById('root'),
    flags: storage.urls
  });

  elmApp.ports.rememberCsvUrl.subscribe(urlToStore => {
    storage.addUrl(urlToStore);
  });
});

const storageKey = "url-list";

class NullStorage {
  static setItem(_) {}
  static getItem(_) { return null; }
}

class AppStorage {
  constructor(storage) {
    this._storage = storage;
  }

  addUrl(url) {
    const newList = this.urls;
    newList.push(url);
    this._storage.setItem(storageKey, JSON.stringify([...new Set(newList)]));
  }

  get urls() {
    return JSON.parse(this._storage.getItem(storageKey)) || [];
  }
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

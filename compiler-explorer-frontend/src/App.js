import './App.css';

import React from 'react';

class App extends React.Component {

  tokenise() {
    const source = document.getElementById("text");
    const target = document.getElementById("tokens");
    fetch("http://127.0.0.1:8080/lex", { method: 'POST', body: source.value })
      .then(resp => resp.text())
      .then(t => target.value = t)
      .then(this.parse)
  }

  parse() {
    const source = document.getElementById("tokens");
    const target = document.getElementById("expressions");
    fetch("http://127.0.0.1:8080/parse", { method: 'POST', body: source.value })
      .then(resp => resp.text())
      .then(t => target.value = t)
  }

  render() {
    return (
      <div className="App">
        <div>
          <textarea id='text' class='editor' rows='14' onChange={e => this.tokenise()}></textarea>
          <textarea id='tokens' class='editor' rows='14' onChange={e => this.parse()}></textarea>
          <textarea id='expressions' class='editor' rows='14'></textarea>
        </div>

      </div>
    );
  }

}

export default App;

import './App.css';

import React from 'react';

class App extends React.Component {

  lexAndParse() {
    const source = document.getElementById("text");
    const target1 = document.getElementById("tokens");
    const target2 = document.getElementById("expressions");

    target1.value = "";
    target2.value = "";

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        target1.value = ts[0];
        target2.value = ts[1];
      })
  }

  render() {
    return (
      <div className="App">
        <div>
          <textarea id='text' class='editor' rows='14' onChange={e => this.lexAndParse()}></textarea>
          <textarea id='tokens' class='editor' rows='14'></textarea>
          <textarea id='expressions' class='editor' rows='14'></textarea>
        </div>

      </div>
    );
  }

}

export default App;

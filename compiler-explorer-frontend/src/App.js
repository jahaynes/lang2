import './App.css';

import React from 'react';

class App extends React.Component {

  lexAndParse() {
    const source  = document.getElementById("text");
    const target1 = document.getElementById("tokens");
    const target2 = document.getElementById("expressions");
    const target3 = document.getElementById("expressionsPretty");
    const target4 = document.getElementById("contified");
    const target5 = document.getElementById("contifiedPretty");
    const target6 = document.getElementById("freeVars");

    target1.value = "";
    target2.value = "";
    target3.value = "";
    target4.value = "";
    target5.value = "";
    target6.value = "";

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        target1.value = ts[0];
        target2.value = ts[1];
        target3.value = ts[2];
        target4.value = ts[3];
        target5.value = ts[4];
        target6.value = ts[5];
      })
  }

  render() {
    return (
      <div className="App">

        <label>Source / Tokens</label>
        <div>
          <textarea id='text' className='editor' spellCheck='false' rows='14' onChange={e => this.lexAndParse()}></textarea>
          <textarea id='tokens' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Definitions / Pretty</label>
        <div>
          <textarea id='expressions' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='expressionsPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Contified / Pretty</label>
        <div>
          <textarea id='contified' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='contifiedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Free Variables</label>
        <div>
          <textarea id='freeVars' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

      </div>
    );
  }

}

export default App;

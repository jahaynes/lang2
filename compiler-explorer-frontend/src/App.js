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
    const target6 = document.getElementById("types");

    target1.value = "";
    target2.value = "";
    target3.value = "";
    target4.value = "";
    target5.value = "";
    target6.value = "";

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        target1.value = ts.tokens;
        target2.value = ts.defns;
        target3.value = ts.prettyDefns;
        target4.value = ts.contified;
        target5.value = ts.prettyContified;
        target6.value = ts.types;
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

        <label>Types</label>
        <div>
          <textarea id='types' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

      </div>
    );
  }

}

export default App;

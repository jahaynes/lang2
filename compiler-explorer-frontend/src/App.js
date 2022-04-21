import './App.css';

import React from 'react';

class App extends React.Component {

  lexAndParse() {
    const source   = document.getElementById("text");
    const target1  = document.getElementById("tokens");
    const target2  = document.getElementById("expressions");
    const target3  = document.getElementById("expressionsPretty");
    const target4  = document.getElementById("typeEnv");
    const target5  = document.getElementById("types");
    const target6  = document.getElementById("etaExpanded");
    const target7  = document.getElementById("saturated");
    const target8  = document.getElementById("discarded");
    const target9  = document.getElementById("contified");
    const target10 = document.getElementById("contifiedPretty");
    const target11 = document.getElementById("optimised");
    const target12 = document.getElementById("optimisedPretty");

    target1.value  = "";
    target2.value  = "";
    target3.value  = "";
    target4.value  = "";
    target5.value  = "";
    target6.value  = "";
    target7.value  = "";
    target8.value  = "";
    target9.value  = "";
    target10.value = "";
    target11.value = "";
    target12.value = "";

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        target1.value = ts.tokens;
        target2.value = ts.defns;
        target3.value = ts.prettyDefns;
        target4.value = ts.typeEnv;
        target5.value = ts.types;
        target6.value = ts.etaExpanded;
        target7.value = ts.saturated;
        target8.value = ts.discarded;
        target9.value = ts.contified;
        target10.value = ts.prettyContified;
        target11.value = ts.optimised;
        target12.value = ts.prettyOptimised;
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

        <label>Types</label>
        <div>
          <textarea id='typeEnv' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='types' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Eta Expanded</label>
        <div>
          <textarea id='etaExpanded' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='discarded' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Saturated</label>
        <div>
          <textarea id='saturated' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Contified / Pretty</label>
        <div>
          <textarea id='contified' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='contifiedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Optimised</label>
        <div>
          <textarea id='optimised' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='optimisedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

      </div>
    );
  }

}

export default App;

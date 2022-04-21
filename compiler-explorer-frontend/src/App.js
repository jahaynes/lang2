import './App.css';

import React from 'react';

class App extends React.Component {

  lexAndParse() {
    const source            = document.getElementById("text");
    const tokens            = document.getElementById("tokens");
    const expressions       = document.getElementById("expressions");
    const expressionsPretty = document.getElementById("expressionsPretty");
    const typeEnv           = document.getElementById("typeEnv");
    const types             = document.getElementById("types");
    const etaExpanded       = document.getElementById("etaExpanded");
    const saturated         = document.getElementById("saturated");
    const discarded         = document.getElementById("discarded");
    const contified         = document.getElementById("contified");
    const contifiedPretty   = document.getElementById("contifiedPretty");
    const optimised         = document.getElementById("optimised");
    const optimisedPretty   = document.getElementById("optimisedPretty");

    tokens.value            = "";
    expressions.value       = "";
    expressionsPretty.value = "";
    typeEnv.value           = "";
    types.value             = "";
    etaExpanded.value       = "";
    saturated.value         = "";
    discarded.value         = "";
    contified.value         = "";
    contifiedPretty.value   = "";
    optimised.value         = "";
    optimisedPretty.value   = "";

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        tokens.value            = ts.tokens;
        expressions.value       = ts.defns;
        expressionsPretty.value = ts.prettyDefns;
        typeEnv.value           = ts.typeEnv;
        types.value             = ts.types;
        etaExpanded.value       = ts.etaExpanded;
        saturated.value         = ts.saturated;
        discarded.value         = ts.discarded;
        contified.value         = ts.contified;
        contifiedPretty.value   = ts.prettyContified;
        optimised.value         = ts.optimised;
        optimisedPretty.value   = ts.prettyOptimised;
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

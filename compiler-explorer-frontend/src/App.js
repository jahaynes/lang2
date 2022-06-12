import './App.css';

import React from 'react';

class App extends React.Component {

  getAndClearElement(elemId) {
    const elem = document.getElementById(elemId);
    elem.value = "";
    return elem;
  }

  lexAndParse() {
    const source        = document.getElementById("text");

    const tokens        = this.getAndClearElement("tokens");
    const defns         = this.getAndClearElement("defns");
    const prettyDefns   = this.getAndClearElement("prettyDefns");
    const callGraph     = this.getAndClearElement("callGraph");
    const typeCheckPlan = this.getAndClearElement("typeCheckPlan");
    const inferred      = this.getAndClearElement("inferred");

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        tokens.value        = ts.tokens;
        defns.value         = ts.defns;
        prettyDefns.value   = ts.prettyDefns;
        callGraph.value     = ts.callGraph;
        typeCheckPlan.value = ts.typeCheckPlan;
        inferred.value      = ts.inferred;
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
          <textarea id='defns' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='prettyDefns' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Call Graph / Typecheck Plan / Inferred</label>
        <div>
          <textarea id='callGraph' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='typeCheckPlan' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='inferred' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

      </div>
    );
  }

}

export default App;

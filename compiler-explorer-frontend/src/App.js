import './App.css';

import React from 'react';

class App extends React.Component {

  getAndClearElement(elemId) {
    const elem = document.getElementById(elemId);
    elem.value = "";
    return elem;
  }

  lexAndParse() {
    const source = document.getElementById("text")

    const tokens           = this.getAndClearElement("tokens")
    const prettyDefns      = this.getAndClearElement("prettyDefns")
    const inferred         = this.getAndClearElement("inferred")
    const inferredPretty   = this.getAndClearElement("inferredPretty")
    const etaExpanded      = this.getAndClearElement("etaExpanded")
    const etaPretty        = this.getAndClearElement("etaPretty")
    const contifiedPretty  = this.getAndClearElement("contifiedPretty")
    const contifiedUntyped = this.getAndClearElement("contifiedUntyped")
    const closureConverted = this.getAndClearElement("closureConverted")

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        tokens.value           = ts.tokens;
        prettyDefns.value      = ts.prettyDefns;
        inferred.value         = ts.inferred;
        inferredPretty.value   = ts.inferredPretty;
        etaExpanded.value      = ts.etaExpanded;
        etaPretty.value        = ts.etaPretty;
        //saturated.value        = ts.saturated;
        //saturatedPretty.value  = ts.saturatedPretty;
        contifiedPretty.value  = ts.contifiedPretty;
        contifiedUntyped.value = ts.contifiedUntyped;
        closureConverted.value = ts.closureConverted;
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
          <textarea id='prettyDefns' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Type Inference / Pretty</label>
        <div>
          <textarea id='inferred' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='inferredPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Eta Expanded</label>
        <div>
          <textarea id='etaExpanded' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='etaPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Contified</label>
        <div>
          <textarea id='contifiedPretty' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='contifiedUntyped' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Closure Converted</label>
        <div>
          <textarea id='closureConverted' className='editor' spellCheck='false' rows='14'></textarea>
        </div>
      </div>
    );
  }

}

export default App;

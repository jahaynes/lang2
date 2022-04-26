import './App.css';

import React from 'react';

class App extends React.Component {

  getAndClearElement(elemId) {
    const elem = document.getElementById(elemId);
    elem.value = "";
    return elem;
  }

  lexAndParse() {
    const source                 = document.getElementById("text");
    const tokens                 = this.getAndClearElement("tokens");
    const expressions            = this.getAndClearElement("expressions");
    const expressionsPretty      = this.getAndClearElement("expressionsPretty");
    const etaExpanded            = this.getAndClearElement("etaExpanded");
    const saturated              = this.getAndClearElement("saturated");
    const contified              = this.getAndClearElement("contified");
    const contifiedPretty        = this.getAndClearElement("contifiedPretty");
    const optimised              = this.getAndClearElement("optimised");
    const optimisedPretty        = this.getAndClearElement("optimisedPretty");
    const closureConverted       = this.getAndClearElement("closureConverted");
    const closureConvertedPretty = this.getAndClearElement("closureConvertedPretty");

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        tokens.value                 = ts.tokens;
        expressions.value            = ts.defns;
        expressionsPretty.value      = ts.prettyDefns;
        etaExpanded.value            = ts.etaExpanded;
        saturated.value              = ts.saturated;
        contified.value              = ts.contified;
        contifiedPretty.value        = ts.prettyContified;
        optimised.value              = ts.optimised;
        optimisedPretty.value        = ts.prettyOptimised;
        closureConverted.value       = ts.closureConverted;
        closureConvertedPretty.value = ts.prettyClosureConverted;
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

        <label>Eta Expanded / Saturated </label>
        <div>
          <textarea id='etaExpanded' className='editor' spellCheck='false' rows='14'></textarea>
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

        <label>Closure Converted</label>
        <div>
          <textarea id='closureConverted' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='closureConvertedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

      </div>
    );
  }

}

export default App;

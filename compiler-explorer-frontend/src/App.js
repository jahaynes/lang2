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

    const tokens                 = this.getAndClearElement("tokens")
    const prettyDefns            = this.getAndClearElement("prettyDefns")
    const inferred               = this.getAndClearElement("inferred")
    const inferredPretty         = this.getAndClearElement("inferredPretty")
    const etaExpanded            = this.getAndClearElement("etaExpanded")
    const etaPretty              = this.getAndClearElement("etaPretty")
    const contified              = this.getAndClearElement("contified")
    const contifiedPretty        = this.getAndClearElement("contifiedPretty")
    const closureConverted       = this.getAndClearElement("closureConverted");
    const closureConvertedPretty = this.getAndClearElement("closureConvertedPretty");
    const lambdaLifted           = this.getAndClearElement("lambdaLifted");
    const lambdaLiftedPretty     = this.getAndClearElement("lambdaLiftedPretty");

    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        tokens.value                 = ts.tokens;
        prettyDefns.value            = ts.prettyDefns;
        inferred.value               = ts.inferred;
        inferredPretty.value         = ts.inferredPretty;
        etaExpanded.value            = ts.etaExpanded;
        etaPretty.value              = ts.etaPretty;
        contified.value              = ts.contified;
        contifiedPretty.value        = ts.contifiedPretty;
        closureConverted.value       = ts.closureConverted;
        closureConvertedPretty.value = ts.closureConvertedPretty;
        lambdaLifted.value           = ts.lambdaLifted;
        lambdaLiftedPretty.value     = ts.lambdaLiftedPretty;
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
          <textarea id='contified' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='contifiedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Closure Converted</label>
        <div>
          <textarea id='closureConverted' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='closureConvertedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Lambda Lifted</label>
        <div>
          <textarea id='lambdaLifted' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='lambdaLiftedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

      </div>
    );
  }

}

export default App;

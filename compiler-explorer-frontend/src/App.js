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

    //const output                 = this.getAndClearElement("output")
    const tokens                 = this.getAndClearElement("tokens")
    const prettyDefns            = this.getAndClearElement("prettyDefns")
    const inferred               = this.getAndClearElement("inferred")
    const inferredPretty         = this.getAndClearElement("inferredPretty")
    const etaExpanded            = this.getAndClearElement("etaExpanded")
    const anfConverted           = this.getAndClearElement("anfConverted")
    const anfPretty              = this.getAndClearElement("anfPretty")
    const closureConverted       = this.getAndClearElement("closureConverted")
    const closureConvertedPretty = this.getAndClearElement("closureConvertedPretty")
    const lambdaLifted           = this.getAndClearElement("lambdaLifted")
    const lambdaLiftedPretty     = this.getAndClearElement("lambdaLiftedPretty")
    const codeGen0               = this.getAndClearElement("codeGen0")
    const codeGen1               = this.getAndClearElement("codeGen1")
    fetch("http://127.0.0.1:8080/lexAndParse", { method: 'POST', body: source.value })
      .then(resp => resp.json())
      .then((ts) => {
        //output.value                 = ts.output;
        tokens.value                 = ts.tokens;
        prettyDefns.value            = ts.prettyDefns;
        inferred.value               = ts.inferred;
        inferredPretty.value         = ts.inferredPretty;
        etaExpanded.value            = ts.etaExpanded;
        anfConverted.value           = ts.anfConverted;
        anfPretty.value              = ts.anfPretty;
        closureConverted.value       = ts.closureConverted;
        closureConvertedPretty.value = ts.closureConvertedPretty;
        lambdaLifted.value           = ts.lambdaLifted;
        lambdaLiftedPretty.value     = ts.lambdaLiftedPretty;
        codeGen0.value               = ts.codeGen0;
        codeGen1.value               = ts.codeGen1;
      })
  }

  render() {
    return (
      <div className="App">

        <label>Source / CodeGen</label>
        <div>
          <textarea id='text' className='editor' spellCheck='false' rows='14' onChange={e => this.lexAndParse()}></textarea>
          <textarea id='codeGen0' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='codeGen1' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Tokens / Definitions</label>
        <div>
          <textarea id='tokens' className='editor' spellCheck='false' rows='14'></textarea>
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
        </div>

        <label>Anf Converted</label>
        <div>
          <textarea id='anfConverted' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='anfPretty' className='editor' spellCheck='false' rows='14'></textarea>
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

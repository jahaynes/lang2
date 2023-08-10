import React from 'react';

class App extends React.Component {

  getAndClearElement(elemId) {
    const elem = document.getElementById(elemId);
    elem.value = "";
    return elem;
  }

  selectExample(e) {
    const source = document.getElementById("text")
    const example = e.target.value;
    if (example) {
      fetch("http://127.0.0.1:8080/example/" + example, { headers: { 'Accept': 'application/json', 'Content-Type': 'application/json' } })
        .then(resp => resp.json())
        .then(out => { source.value = out })
        .then(_ => this.lexAndParse());
    }
  }

  run() {

    this.lexAndParse();

    const debugLog = this.getAndClearElement("debugLog");
    const output   = this.getAndClearElement("output");

    fetch("http://127.0.0.1:8080/run", { method:  'POST'
                                       , headers: { 'Accept': 'application/json'
                                                  , 'Content-Type': 'application/json' } })
      .then(resp => resp.json())
      .then(out => {
        debugLog.value = out[0];
        output.value   = out[1];
      });
  }

  lexAndParse() {
    const source = document.getElementById("text")

    const prettyDefns            = this.getAndClearElement("prettyDefns")
    const inferredPretty         = this.getAndClearElement("inferredPretty")
    const etaExpanded            = this.getAndClearElement("etaExpanded")
    const anfPretty              = this.getAndClearElement("anfPretty")
    const closureConvertedPretty = this.getAndClearElement("closureConvertedPretty")
    const lambdaLiftedPretty     = this.getAndClearElement("lambdaLiftedPretty")
    const uncurriedPretty        = this.getAndClearElement("uncurriedPretty")
    const codeGenA               = this.getAndClearElement("codeGenA")

    const req = { getInput: source.value }

    fetch("http://127.0.0.1:8080/lexAndParse", { method:  'POST'
                                               , headers: { 'Accept': 'application/json'
                                                          , 'Content-Type': 'application/json' }
                                               , body:    JSON.stringify(req)
                                               })
      .then(resp => resp.json())
      .then(ts => {
        prettyDefns.value            = ts.prettyDefns;
        inferredPretty.value         = ts.inferredPretty;
        etaExpanded.value            = ts.etaExpanded;
        anfPretty.value              = ts.anfPretty;
        closureConvertedPretty.value = ts.closureConvertedPretty;
        lambdaLiftedPretty.value     = ts.lambdaLiftedPretty;
        uncurriedPretty.value        = ts.uncurriedPretty;
        codeGenA.value               = ts.codeGenA; })
      .catch(exception => console.log(exception));
  }

  render() {

    const numRows = 12;

    return (
      <div className="App">

        <div>
          <select onChange={e => this.selectExample(e)}>
            <option></option>
            <option id="closure">closure</option>
            <option id="summorial">summorial</option>
            <option id="pair">pair</option>
          </select>
        </div>

        <label>Source / CodegenA </label>
        <div>
          <textarea id='text' className='editor' spellCheck='false' rows={numRows} onChange={e => this.lexAndParse()}></textarea>
          <textarea id='codeGenA' className='editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <div>
          <label>Output</label>
          <button id='exec' onClick={e => this.run()}>Run</button>
          <button id='stop' onClick={e => alert('stop')}>Stop All</button>
        </div>
        <div>
          <textarea id='debugLog' className='editor' spellCheck='false' rows={numRows}></textarea>
          <textarea id='output' className='editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Lambda Lifted / Uncurried</label>
        <div>
          <textarea id='lambdaLiftedPretty' className='editor' spellCheck='false' rows={numRows}></textarea>
          <textarea id='uncurriedPretty' className='editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Closure Converted / Examples </label>
        <div>
          <textarea id='closureConvertedPretty' className='editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Anf Converted</label>
        <div>
          <textarea id='anfPretty' className='editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Eta Expanded</label>
        <div>
          <textarea id='etaExpanded' className='editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Type Inference / Pretty</label>
        <div>
          <textarea id='inferredPretty' className='editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Tokens / Definitions</label>
        <div>
          <textarea id='prettyDefns' className='editor' spellCheck='false' rows={numRows}></textarea>
        </div>
      </div>
    );
  }
}

export default App;

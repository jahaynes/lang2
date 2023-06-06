import './App.css';

import React from 'react';

class App extends React.Component {

  getAndClearElement(elemId) {
    const elem = document.getElementById(elemId);
    elem.value = "";
    return elem;
  }

  selectExample(e) {

    const source = document.getElementById("text")

    switch (e.target.value) {
      case "closure":
        fetch( "http://127.0.0.1:8080/example/closure", { headers: { 'Accept': 'application/json', 'Content-Type': 'application/json' } })
          .then(resp => resp.json())
          .then(out => {source.value = out})
          .then(_ => this.lexAndParse());
        break;
      default:
        break;
    }
  }

  run() {

    this.lexAndParse();

    const output = this.getAndClearElement("output");

    fetch("http://127.0.0.1:8080/run", { method:  'POST'
                                       , headers: { 'Accept': 'application/json'
                                                  , 'Content-Type': 'application/json' } })
      .then(resp => resp.json())
      .then(out => output.value = out);
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
    const codeGen0               = this.getAndClearElement("codeGen0")
    const codeGen1               = this.getAndClearElement("codeGen1")

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
        codeGen0.value               = ts.codeGen0;
        codeGen1.value               = ts.codeGen1; })
      .catch(exception => console.log(exception));
  }

  render() {
    return (
      <div className="App">

        <div>
          <select onChange={e => this.selectExample(e)}>
            <option></option>
            <option id="closure">closure</option>
          </select>
        </div>

        <label>Source / Codegen0 </label>
        <div>
          <textarea id='text' className='editor' spellCheck='false' rows='14' onChange={e => this.lexAndParse()}></textarea>
          <textarea id='codeGen0' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <div>
          <label>Codegen1 / Output</label>
          <button id='exec' onClick={e => this.run()}>Run</button>
          <button id='stop' onClick={e => alert('stop')}>Stop All</button>
        </div>
        <div>
          <textarea id='codeGen1' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='output' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Lambda Lifted / Uncurried</label>
        <div>
          <textarea id='lambdaLiftedPretty' className='editor' spellCheck='false' rows='14'></textarea>
          <textarea id='uncurriedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Closure Converted / Examples </label>
        <div>
          <textarea id='closureConvertedPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Anf Converted</label>
        <div>
          <textarea id='anfPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Eta Expanded</label>
        <div>
          <textarea id='etaExpanded' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Type Inference / Pretty</label>
        <div>
          <textarea id='inferredPretty' className='editor' spellCheck='false' rows='14'></textarea>
        </div>

        <label>Tokens / Definitions</label>
        <div>
          <textarea id='prettyDefns' className='editor' spellCheck='false' rows='14'></textarea>
        </div>
      </div>
    );
  }
}

export default App;

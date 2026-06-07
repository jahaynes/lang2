import React from 'react';

class App extends React.Component {

  componentDidMount() {
    this.populateExamples();
  }

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
    const codeGenD               = this.getAndClearElement("codeGenD")

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
        codeGenD.value               = ts.codeGenD; })
      .catch(exception => console.log(exception));
  }

  render() {

    const numRows = 16;

    return (
      <div className="App">

        <div>
          <select id='examples' onChange={e => this.selectExample(e)}>
          </select>
        </div>

        <label>Source</label>
        <div>
          <textarea id='text' className='double editor' spellCheck='false' rows={numRows} onChange={e => this.lexAndParse()}></textarea>
        </div>

        <label>CodegenD</label>
        <div>
          <textarea id='codeGenD' className='double editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <div>
          <label>Output</label>
          <button id='exec' onClick={e => this.run()}>Run</button>
          <button id='stop' onClick={e => alert('stop')}>Stop All</button>
        </div>
        <div>
          <textarea id='debugLog' className='single editor' spellCheck='false' rows={numRows}></textarea>
          <textarea id='output' className='single editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Lambda Lifted / Closure Converted</label>
        <div>
          <textarea id='lambdaLiftedPretty' className='single editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Anf Converted / Eta Expanded</label>
        <div>
          <textarea id='closureConvertedPretty' className='single editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Anf Converted</label>
        <div>
          <textarea id='anfPretty' className='single editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Eta Expanded</label>
        <div>
          <textarea id='etaExpanded' className='single editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Type Inference / Pretty</label>
        <div>
          <textarea id='inferredPretty' className='single editor' spellCheck='false' rows={numRows}></textarea>
        </div>

        <label>Tokens / Definitions</label>
        <div>
          <textarea id='prettyDefns' className='single editor' spellCheck='false' rows={numRows}></textarea>
        </div>
      </div>
    );
  }

  populateExamples() {
    const select = document.getElementById('examples');

    fetch("http://127.0.0.1:8080/list-examples", {  headers: { 'Accept': 'application/json'
                                                             , 'Content-Type': 'application/json' } })
        .then(resp => resp.json())
        .then(examples => {
          select.innerHTML = '';
          const emptyOption = document.createElement('option');
          select.appendChild(emptyOption);
          examples.forEach(name => {
            const option = document.createElement('option');
            option.value = name;
            option.textContent = name;
            select.appendChild(option);
          });
        })
        .catch(err => console.log('Failed to load examples:', err));
  }

}

export default App;

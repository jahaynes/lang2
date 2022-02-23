import './App.css';

function App() {
  return (
    <div className="App">
      <table>
        <tr>
          <td><textarea rows='10'></textarea></td>
          <td><textarea rows='10' readonly='true'></textarea></td>
        </tr>
        <tr>
          <td><textarea rows='10' readonly='true'></textarea></td>
          <td><textarea rows='10' readonly='true'></textarea></td>
        </tr>
      </table>

    </div>
  );
}

export default App;

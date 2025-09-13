import React, { useState, useEffect, useRef, useReducer } from 'react';
import * as GCodePreview from 'gcode-preview';
import { useViewer } from "core/GCodeViewer/StateMachines/Viewer"
import { mkMsg, useStateMachineApp } from "core/GCodeViewer/StateMachines/App"
import { trunc, toNumber } from "core/Data/Int";
import { mkRemoteData, onRemoteData, RemoteData } from 'core/GCodeViewer/RemoteData';


const App2: React.FC<{ data: State["index"] }> = ({ data }) => {

  return (
    <div>
      <h1>App2</h1>
    </div>
  )
}

type State = (ReturnType<typeof useStateMachineApp>)["state"]

const App: React.FC = () => {

  const { state, dispatch } = useStateMachineApp()

  useEffect(() => {
    console.log("render", state);
  });

  useEffect(() => {
    dispatch.runFetchIndex({ url: "/out/index.json" })
  }, []);

  return (
    <div>
      {onRemoteData(state.index, {
        NotAsked: () => <>"Not Asked"</>,
        Loading: () => <>"Loading"</>,
        Loaded: (data) => <App2 data={data} />,
        Error: (err) => <>"Error: " + err</>
      })}
    </div>
  )

  // const [state, dispatch] = useStateMachine(Purs.tsApi)

  // useEffect(() => {
  //   dispatch.loadGcodeLines({ url: "/out/001.gcode" });

  // }, []);

  // return (
  //   <div>
  //     {state.gcodeLines.value.length}
  //     <button onClick={() => dispatch.loadGcodeLines({ url: "/out/001.gcode" })}>Load Gcode</button>
  //     <br />
  //     {onRemoteDataStatus({
  //       notAsked: "Not Asked",
  //       loading: "Loading",
  //       loaded: "Loaded",
  //       error: (err) => "Error: " + err.message
  //     })(state.gcodeLines.status)}
  //   </div>)
};

export default App;

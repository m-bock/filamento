import React, { useState, useEffect, useRef, useReducer } from 'react';
import * as GCodePreview from 'gcode-preview';
import { useViewer } from "core/GCodeViewer/StateMachines/Viewer"
import { onRemoteDataStatus } from 'core/GCodeViewer/RemoteData';
import { trunc, toNumber } from "core/Data/Int";



const App: React.FC = () => {

  const { state, dispatch } = useViewer()

  useEffect(() => {
    dispatch.runLoadGcodeLines({ url: "/out/001.gcode" });
  }, []);

  useEffect(() => {
    console.log("render", state);
  }, []);

  return (
    <div>
      <h1>Hello World</h1>
      {toNumber(state.startLayer)}
      <br />
      {toNumber(state.endLayer)}
      <br />
      <button onClick={() => dispatch.emitSetStartLayer(trunc(10))}>set to 10</button>
      <button onClick={() => dispatch.emitSetStartLayer(trunc(20))}>set to 20</button>
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

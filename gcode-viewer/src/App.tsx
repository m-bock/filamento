import React, { useState, useEffect, useRef, useReducer } from 'react';
import * as GCodePreview from 'gcode-preview';
import { useViewer } from "core/StateMachines/Viewer"
import { onRemoteDataStatus } from 'core/RemoteData';


const App: React.FC = () => {

  const { state, dispatch } = useViewer()

  useEffect(() => {
    dispatch.runLoadGcodeLines({ url: "/out/001.gcode" });
  }, []);

  return (
    <div>
      <h1>Hello World</h1>
      <button onClick={() => dispatch.emitSetStartLayer(round(10))}>Load Gcode</button>
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

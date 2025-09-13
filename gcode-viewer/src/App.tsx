import React, { useState, useEffect, useRef, useReducer } from 'react';
import * as GCodePreview from 'gcode-preview';
import { useViewer } from "core/GCodeViewer/StateMachines/Viewer"
import { mkMsg, useStateMachineApp } from "core/GCodeViewer/StateMachines/App"
import { trunc, toNumber } from "core/Data/Int";
import { mkRemoteData, onRemoteData, RemoteData } from 'core/GCodeViewer/RemoteData';
import { IndexFileItem } from 'core/TypeAliases';


const App3: React.FC<{ data: IndexFileItem }> = ({ data }) => {

  const { state, dispatch } = useViewer()

  useEffect(() => {
    dispatch.runLoadGcodeLines({ url: data.gcode })
  }, [data.gcode])

  return (
    <div>
      {data.name}
    </div>
  )
}


const App2: React.FC<{ data: IndexFileItem[] }> = ({ data }) => {

  return (
    <div>
      {data.map((item) => <App3 data={item} />)}
    </div>
  )
}


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
        NotAsked: () => <>Not Asked</>,
        Loading: () => <>Loading</>,
        Loaded: (data) => <App2 data={data} />,
        Error: (err) => <>{"Error: " + err}</>
      })}
    </div>
  )
}

export default App;

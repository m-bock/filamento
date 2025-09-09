import React, { useState, useEffect, useRef, useReducer } from 'react';
import * as GCodePreview from 'gcode-preview';
import './style.css';
import { Slider } from './Slider';
import * as Purs from "../purs/output/GCodeViewer.StateMachine"
import { useStateMachine } from './Lib';

interface IndexFileItem {
  name: string;
  gcode: string;
  pictures: string[];
};


const GCodeViewer = ({ item, loadedCode }: { item: IndexFileItem, loadedCode?: string[] }) => {
  const canvasRef = useRef<HTMLCanvasElement>(null);

  useEffect(() => {
    if (canvasRef.current) {
      const canvas = canvasRef.current;

      const gcodePreview = GCodePreview.init({
        canvas,
        lineWidth: 4,
        lineHeight: 4,
        renderTravel: false,
        renderExtrusion: true,
        renderTubes: false,
        startLayer: 10,
        endLayer: 20,
        backgroundColor: 'black',
        buildVolume: {
          x: 220,
          y: 220,
          z: 0,
        },
      });


      gcodePreview.processGCode(loadedCode || []);
    }
  }, [loadedCode]);

  return (
    <div className="viewer-item">
      <h3>{item.name}</h3>
      <div className="gcode-path">G-code: {item.gcode}</div>
      {item.pictures.length > 0 && (
        <div className="pictures-container">
          {item.pictures.map((picture, index) => (
            <img key={index} src={picture} alt={`Preview ${index + 1}`} />
          ))}
        </div>
      )}
      {/* <code>{loadedCode}</code> */}

      <div style={{ position: 'relative', width: '100%', height: '500px' }}>
        <div style={{ position: 'absolute', top: 0, left: 0, right: 0, bottom: 0 }}>
          <canvas ref={canvasRef} />
        </div>

        <div style={{ position: 'absolute', bottom: 20, top: 20, right: 10 }}>
          <Slider />
        </div>
      </div>
    </div>
  )
}

const ManyViewers = ({ index }: { index: IndexFileItem[] }) => {
  const [loadedCode, setLoadedCode] = useState<Map<string, string[]>>(new Map());

  useEffect(() => {
    const fetchCode = async () => {
      for (const item of index) {
        const response = await fetch(`${baseUrl}/${item.gcode}`).then(response => response.text());
        setLoadedCode(prev => {
          const newMap = new Map(prev);
          newMap.set(item.name, response.split('\n'));
          return newMap;
        });
      }
    }
    fetchCode();
  }, [index]);

  return (
    <div>
      <div style={{ width: '800px' }} className="main-content">
        {index.map(item => {
          return <GCodeViewer key={item.name} item={item} loadedCode={loadedCode.get(item.name)} />
        })}
      </div>
    </div>

  )
}

const baseUrl = "/out";
const indexUrl = `${baseUrl}/index.json`;

const App2: React.FC = () => {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const previewRef = useRef<any>(null);

  const [indexFile, setIndexFile] = useState<IndexFileItem[] | null>(null);

  useEffect(() => {
    fetch(indexUrl).then(response => response.json()).then(data => {
      setIndexFile(data as IndexFileItem[]);
    });
  }, []);


  return indexFile ? <ManyViewers index={indexFile} /> : <div>Loading...</div>
};



const App: React.FC = () => {

  const [state, dispatch] = useStateMachine(Purs.tsApi)

  return <div>
    <br />

    <button onClick={() => dispatch.disp1()}>Fetch Index File</button>

  </div>
};

export default App;

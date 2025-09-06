import React, { useState, useEffect, useRef } from 'react';
import * as GCodePreview from 'gcode-preview';
import './style.css';
import GCodeViewer from './GCodeViewer';

interface IndexFileItem {
  name: string;
  gcode: string;
  pictures: string[];
};


const MyViewerItem = ({ item, loadedCode }: { item: IndexFileItem, loadedCode?: string }) => {
  return <div className="viewer-item">
    <h3>{item.name}</h3>
    <div className="gcode-path">G-code: {item.gcode}</div>
    {item.pictures.length > 0 && (
      <div className="pictures-container">
        {item.pictures.map((picture, index) => (
          <img key={index} src={picture} alt={`Preview ${index + 1}`} />
        ))}
      </div>
    )}
    <code>{loadedCode}</code>
    <GCodeViewer gcode={loadedCode || ''} />
  </div>
}

const MyViewer = ({ index }: { index: IndexFileItem[] }) => {
  const [loadedCode, setLoadedCode] = useState<Map<string, string>>(new Map());

  useEffect(() => {
    const fetchCode = async () => {
      for (const item of index) {
        const response = await fetch(`${baseUrl}/${item.gcode}`).then(response => response.text());
        setLoadedCode(prev => {
          const newMap = new Map(prev);
          newMap.set(item.name, response);
          return newMap;
        });
      }
    }
    fetchCode();
  }, [index]);

  return <div className="main-content">
    {index.map(item => {
      return <MyViewerItem key={item.name} item={item} loadedCode={loadedCode.get(item.name)} />
    })}
  </div>
}

const baseUrl = "/out";
const indexUrl = `${baseUrl}/index.json`;

const App: React.FC = () => {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const previewRef = useRef<any>(null);

  const [indexFile, setIndexFile] = useState<IndexFileItem[] | null>(null);

  useEffect(() => {
    fetch(indexUrl).then(response => response.json()).then(data => {
      setIndexFile(data as IndexFileItem[]);
    });
  }, []);


  return indexFile ? <MyViewer index={indexFile} /> : <div>Loading...</div>
};

export default App;

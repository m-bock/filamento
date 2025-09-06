import React, { useRef, useEffect } from 'react';
import * as GCodePreview from 'gcode-preview';

interface GCodeViewerProps {
    gcode: string[];
}

const GCodeViewer = ({ gcode }: GCodeViewerProps) => {
    const canvasRef = useRef<HTMLCanvasElement>(null);

    useEffect(() => {
        if (canvasRef.current) {
            const canvas = canvasRef.current;
            const gcodePreview = GCodePreview.init({
                canvas,
                lineWidth: 2,
                lineHeight: 2,
                renderTravel: false,
                renderExtrusion: true,
                renderTubes: false,
                buildVolume: {
                    x: 220,
                    y: 220,
                    z: 0,
                },
            });


            gcodePreview.processGCode(gcode.splice(100, 200));
        }
    }, [gcode]);

    return <canvas ref={canvasRef} />
}

export default GCodeViewer;
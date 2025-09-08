import React, { useRef, useEffect } from 'react';

import { Slider as RadixSlider } from 'radix-ui';
import styles from './Slider.module.css';

interface SliderProps {

}

export const Slider = (props: SliderProps) => {
    return (
        <RadixSlider.Root
            className={styles.Root}
            orientation="vertical"
            defaultValue={[50]}
            max={100}
            step={1}>
            <RadixSlider.Track className={styles.Track}>
                <RadixSlider.Range className={styles.Range} />
            </RadixSlider.Track>
            <RadixSlider.Thumb className={styles.Thumb} aria-label="Volume" />
        </RadixSlider.Root>
    )
}


class Object {
}

class System {
    true: boolean;
    false: boolean;
    null: Object;
    undefined: Object;
    document: Document;
    console: Console;

    __Add(a: number, b: number): number {
        return 0;
    }

    __Mns(a: number, b: number): number {
        return 0;
    }

    __Mul(a: number, b: number): number {
        return 0;
    }

    __Div(a: number, b: number): number {
        return 0;
    }

    __Mod(a: number, b: number): number {
        return 0;
    }

    __Inc(a: number): number {
        return 0;
    }

    __Dec(a: number): number {
        return 0;
    }

    __BitOr(a: number): number {
        return 0;
    }
}

class Console {
    log(msg: string) { }
}

class Array<T> {
    length: number;
    push(o: T) { }
    pop(): T {
        return null;
    }
}

class string {
}

class char {
}

class number {
}

class int extends number {
}

class Type {
}

class boolean {
}

class Math {
    static PI: number;
    sqrt(x: number): number {
        return 0;
    }
    cos(x: number): number {
        return 0;
    }
    sin(x: number): number {
        return 0;
    }
    max(x: number, y:number): number {
        return 0;
    }
}

class CanvasRenderingContext2D {
    fillStyle: string;
    strokeStyle: string;
    font: string;
    textBaseline: string;

    beginPath() { }
    moveTo(x: number, y: number) { }
    lineTo(x: number, y: number) { }
    closePath() { }
    stroke() { }
    fillRect(x: number, y: number, w:number, h:number) { }
    strokeRect(x: number, y: number, w: number, h: number) { }
    clearRect(x: number, y: number, w: number, h: number) { }
    arc(x: number, y: number, radius: number, startAngle: number, endAngle: number, anticlockwise: boolean) { }
    fill() { }
    strokeText(text: string, x: number, y: number, maxWidth: number) { }
    fillText(text: string, x: number, y: number, maxWidth: number) { }
    drawImage(image: HTMLImageElement, offsetX: number, offsetY: number) { }
    save() { }
    restore() { }
    translate(x: number, y: number) { }
    rotate(angle: number) { }
    scale(x: number, y: number) { }
    setTransform(m11: number, m12: number, m21: number, m22: number, dx: number, dy: number) { }
}

class HTMLElement {
}

class HTMLCanvasElement extends HTMLElement {
    height: number;
    width: number;

    getContext(s: string): CanvasRenderingContext2D {
        return null;
    }
}

class HTMLImageElement extends HTMLElement {
    src: string;
    width: number;
    height: number;
}

class Document {
    getElementById(id: string): HTMLElement {
        return null;
    }
}

class Element {
}

class EventTarget {
}

class MouseEvent {
    altKey: boolean;
    button: number;
    buttons: number;
    clientX: number;
    clientY: number;
    ctrlKey: boolean;
    fromElement: Element;
    layerX: number;
    layerY: number;
    metaKey: boolean;
    movementX: number;
    movementY: number;
    offsetX: number;
    offsetY: number;
    pageX: number;
    pageY: number;
    relatedTarget: EventTarget;
    screenX: number;
    screenY: number;
    shiftKey: boolean;
    toElement: Element;
    which: number;
    x: number;
    y: number;
}

class ClientRect {
    bottom: number;
    height: number;
    left: number;
    right: number;
    top: number;
    width: number;
}

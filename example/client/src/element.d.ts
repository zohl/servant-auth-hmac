type InsertAdjacentHTMLPosition = 'beforebegin' | 'afterbegin' | 'beforeend' | 'afterend';

interface Element {
    insertAdjacentHTML:(position: InsertAdjacentHTMLPosition, text: string) => void;
}

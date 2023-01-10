import { readFile } from "fs";
import _ from "lodash";
const { maxBy, shuffle, groupBy, range } = _;

type Entry = {
  className: string,
  attributes: string[]
}

type Dataset = Entry[]

const possitiveClass = 'recurrence-events';
const negativeClass = 'no-recurrence-events';
const fileName = 'static/breast-cancer.data';
const minK = 5;
const numberOfAttributes = 9;


function readData(): Promise<string[]> {
  return new Promise((resolve) => {
    readFile(fileName, (_, data) => {
      resolve(data.toString().split('\n'))
    })
  });
}

async function parseData()
  : Promise<{ className: string, attributes: string[] }[]> {
  const content = await readData();
  const lines = content.map(l => l.split(','));
  return lines.map(([className, ...attributes]) => {
    return { className, attributes };
  });
}

// Fills in missing data with the mode of the coresponding attribute
function fillMissing(dataset: Dataset): Dataset {
  const modes =
    range(numberOfAttributes)
      .map(i => {
        const group = groupBy(dataset, entry => entry.attributes[i])
        return maxBy(_.toPairs(group), ([_, vs]) => vs.length)[0];
      });

  const fillEntry = ({ className, attributes }) => {
    range(numberOfAttributes).forEach(i => {
      if (attributes[i] === '?') {
        attributes[i] = modes[i];
      }
    });
    return { className, attributes };
  };

  return dataset.map(fillEntry);
}

function entropy(dataset: Dataset) {
  if (dataset.length === 0) return 0;

  const total = dataset.length;
  const positive =
    dataset.filter(i => i.className === possitiveClass).length;

  const p = positive / total;
  const q = (total - positive) / total;

  // log2 is NaN if the argument is a 0
  if (p === 0) return - q * Math.log2(q);
  if (q === 0) return - p * Math.log2(p);

  return -p * Math.log2(p) - q * Math.log2(q);
}

// TODO
function getSetIndexes(dataset: Dataset, folds: number): number[] {
  const setIndexes = [];
  const remainder = dataset.length % folds;

  let setSize = Math.floor(dataset.length / folds);
  let increment = 0;

  for (let i = 0; i < folds; i++) {
    let val = i * setSize + increment;
    if (i < remainder) {
      increment++;
    }
    setIndexes.push(val);
  }
  setIndexes.push(dataset.length);
  return setIndexes;
}

function getBestAttribute(
  dataset: Dataset,
  attributeValues: string[][],
  used: number[]
) {
  const datasetEntropy = entropy(dataset);
  const attributes =
    range(numberOfAttributes)
      .filter(i => !used.includes(i))
      .map(index => {
        let currentGain = datasetEntropy;
        attributeValues[index].forEach((v: string) => {
          const filtered =
            dataset.filter(entry => entry.attributes[index] === v);
          const datasetEntropy = entropy(filtered);
          currentGain -= filtered.length / dataset.length * datasetEntropy;
        })
        return { gain: currentGain, index };
      })

  return maxBy(attributes, a => a.gain).index;
}

function getMajorityClass(dataset: Dataset) {
  const positive =
    dataset.filter(e => e.className === possitiveClass).length;
  const negative =
    dataset.filter(e => e.className === negativeClass).length;
  return positive > negative ? possitiveClass : negativeClass;
}

//TODO
function buildTree(
  root,
  dataset: Dataset,
  used,
  attributeValues: string[][]
) {
  const datasetEntropy = entropy(dataset);
  if (datasetEntropy === 0 || dataset.length <= minK) {
    root.attribute = -1;
    root.className =
      dataset.length
        ? getMajorityClass(dataset)
        : shuffle([possitiveClass, negativeClass])[0];
    return;
  }
  const bestIndex = getBestAttribute(dataset, attributeValues, used);
  root.nodes = new Map();
  root.attribute = bestIndex;
  attributeValues[bestIndex].forEach(v => {
    const subset = dataset.filter(i => i.attributes[bestIndex] === v)
    const node = {};
    buildTree(node, subset, [...used, bestIndex], attributeValues);
    root.nodes.set(v, node);
  });
}

function testEntry(root, attributes: string[]) {
  if (root.attribute === -1) {
    return root.className;
  }
  return testEntry(root.nodes.get(attributes[root.attribute]), attributes);
}

// TODO
function testModel(model, testData: Dataset) {
  const matches = testData.map(entry => {
    const result = testEntry(model, entry.attributes);
    return result === entry.className;
  }).filter(m => m);
  return matches.length / testData.length;
}

// TODO
function crossValidate(
  dataset: Dataset,
  folds: number,
  attributeValues: string[][]
): number {
  const databackup = shuffle(dataset);

  const setIndexes = getSetIndexes(dataset, folds);
  let total = 0;
  for (let i = 0; i < folds; i++) {
    const testData =
      databackup
        .filter(
          (_, index) => setIndexes[i] <= index && index < setIndexes[i + 1]);
    const buildData =
      databackup
        .filter((_, index) => !(setIndexes[i] <= index && index < setIndexes[i + 1]));

    const model = {};
    buildTree(model, buildData, [], attributeValues);

    const accuracy = testModel(model, testData);
    total += accuracy;
    console.log(i, accuracy);
  }

  return total / folds;
}

function getAttributeValues(dataset: Dataset): string[][] {
  return range(numberOfAttributes)
    .map(i => new Set(dataset.map(entry => entry.attributes[i])))
    .map(set => Array.from(set.keys()));
}

async function main() {
  const folds = 10;
  const dataset = await parseData();
  const totalDataset = fillMissing(dataset);
  const attributeValues = getAttributeValues(dataset);
  console.log(crossValidate(totalDataset, folds, attributeValues));
}

main();

// TODOs:
// - type for "root"/"model"

import { readFile } from "fs";
import _ from "lodash";
const { maxBy, shuffle, groupBy, values, range } = _;

type Dataset = {
  className: string,
  attributes: string[]
}[]

const possitiveClass = 'recurrence-events';
const negativeClass = 'no-recurrence-events';
const fileName = 'static/breast-cancer.data';
const minK = 5;
const folds = 10;
const numberOfAttributes = 9;
const root = {};
let dataset: Dataset;
let attributeValues;
let databackup;


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
  // this.fillMissing();
  // this.attributeValues = Array.from(Array(this.numberOfAttributes).keys())
  //   .map(i => new Set(this.dataset.map(inst => inst.attributes[i])))
  //   .map(set => Array.from(set.keys()))
}

// TODO fill in missing with the most common measurments
function fillMissing(dataset: Dataset): Dataset {
  // Group by each attribute
  // group_by_attribute := {attr_value => entries_with_that_value}
  // modes := group_by_attribute foreach attribute, attribute = index
  const modes =
    range(numberOfAttributes)
      .map(i => groupBy(dataset, entry => entry.attributes[i]));
  console.log(modes);
  return [];

  // range(numberOfAttributes).forEach(i => {
  //   const group = groupBy(dataset, entry => entry.attributes[i]);
  //   const mostCommon = maxBy(values(group), g => g.length)[0].attributes[i];
  //   dataset = dataset.map(entry => {
  //     if (entry.attributes[i] === '?') {
  //       entry.attributes[i] = mostCommon;
  //     }
  //     return entry;
  //   });
  // });
}

async function main() {
  const dataset = await parseData();
  console.log(dataset);
  const totalData = fillMissing(dataset);
}

main();

// TODOs:
// -- fill missing data (find out how)
// -- don't use global variables

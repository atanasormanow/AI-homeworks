import { readFile } from "fs";
import _, { forEach } from "lodash";
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


async function main() {
  const dataset = await parseData();
  // console.log(dataset);
  const totalData = fillMissing(dataset);
}

main();

// TODOs:
// -- don't use global variables

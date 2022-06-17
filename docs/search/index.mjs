import algoliasearch from 'algoliasearch';
import fs from 'fs/promises';

const indexJson = JSON.parse(await fs.readFile('./index.json'));

const key = process.env['ALGOLIA_KEY'];
if ( ! key ) { throw Error(`No key!`); }
const client = algoliasearch('M53HHHS0ZW', key);
const index = client.initIndex('docs');

const now = Date.now();
const indexJson_lu = indexJson.map((x) => ({ ...x, lu: now }));

console.log('Running!', indexJson.length);
const r_puo = await index.partialUpdateObjects(indexJson_lu, {
  createIfNotExists: true,
});
console.log(r_puo);
const r_db = await index.deleteBy({
  numericFilters: [ `lu < ${now}` ],
});
console.log(r_db);
console.log('Done!');

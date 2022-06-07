import algoliasearch from 'algoliasearch';
import fs from 'fs/promises';

const indexJson = JSON.parse(await fs.readFile('./index.json'));
const discussionsJson = JSON.parse(await fs.readFile('./discussions.json'));

const key = process.env['ALGOLIA_KEY'];
if ( ! key ) { throw Error(`No key!`); }
const client = algoliasearch('M53HHHS0ZW', key);
const index = client.initIndex('docs');
const discussionsIndex = client.initIndex('discussions');

const now = Date.now();
const indexJson_lu = indexJson.map((x) => ({ ...x, lu: now }));
const discussionsJson_lu = discussionsJson.map((x) => ({ ...x, lu: now }));

console.log('Running!', indexJson.length);
const r_puo = await index.partialUpdateObjects(indexJson_lu, {
  createIfNotExists: true,
});
console.log(r_puo);
const r_db = await index.deleteBy({
  numericFilters: [ `lu < ${now}` ],
});
console.log(r_db);
console.log('Running on discussions', discussionsJson_lu.length);
const d_puo = await discussionsIndex.partialUpdateObjects(discussionsJson_lu, {
  createIfNotExists: true,
});
console.log(d_puo);
const d_db = await discussionsIndex.deleteBy({
  numericFilters: [ `lu < ${now}` ],
});
console.log(d_db);
console.log('Done!');

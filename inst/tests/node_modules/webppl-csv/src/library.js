var fs = require('fs');
var babyparse = require('babyparse');

// can work with an erp of query.tables (useful for data analysis models)
var writeQueryERP = function(erp, filename, header) {
 var supp = erp.support([]);
 var csvFile = fs.openSync(filename, 'w');
 fs.writeSync(csvFile,header + ',Probability\n')
 supp.forEach(function(s) {supportWriter(s, Math.exp(erp.score([], s)), csvFile);})
 fs.closeSync(csvFile);
};

var supportWriter = function(s, p, handle) {
 var sLst = _.pairs(s);
 var l = sLst.length;
 for (var i = 0; i < l; i++) {
   fs.writeSync(handle, sLst[i].join(',')+','+p+'\n');
 }
};

function readCSV(filename){
  return babyparse.parse(fs.readFileSync(filename, 'utf8')).data;
};

function writeCSV(jsonCSV, filename){
  fs.writeFileSync(filename, babyparse.unparse(jsonCSV) + "\n");
};

// for more manual file writing control
var openFile = function(filename) {
 var csvFile = fs.openSync(filename, 'w');
 return csvFile
};

var writeLine = function(handle, line){
  fs.writeSync(handle, line+'\n');
};

var writeMarginals = function(handle,erp) {
   var supp = erp.support([]);
   supp.forEach(function(s) {supportWriter(s, Math.exp(erp.score([], s)), handle);})
};

var writeJoint = function(handle,erp) {
   var supp = erp.support([]);
   _.isObject(supp[0]) ? writeLine(handle, [_.keys(supp[0]),"Probability"].join(',')) : null
   supp.forEach(function(s) {writeLine(handle, [_.values(s), Math.exp(erp.score([], s))].join(','));})
};

var closeFile = function(handle){
 fs.closeSync(handle);
};

module.exports = {
  readCSV: readCSV,
  writeCSV: writeCSV,
  writeMarginals:writeMarginals,
  writeJoint: writeJoint,
  openFile: openFile,
  closeFile: closeFile,
  writeLine: writeLine,
  writeQueryERP:writeQueryERP
};

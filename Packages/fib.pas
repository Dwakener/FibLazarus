{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FIB;

{$warn 5023 off : no warning about unused units}
interface

uses
  DBParsers, DSContainer, FIB_FMX_DBLoginDlg, FIBCacheManage, 
  FIBCloneComponents, FIBConsts, FIBDatabase, FIBDataSet, FIBDBLoginDlg, 
  FIBMiscellaneous, FIBPlatforms, FIBQuery, FIBSafeTimer, FIBSQLMonitor, 
  IB_ErrorCodes, IB_Externals, IB_Install, IB_InstallHeader, IB_Intf, 
  IB_Services, ibase, IBBlobFilter, pFIBArray, pFIBCacheQueries, 
  pFIBClientDataSet, pFIBDatabase, pFIBDataInfo, pFIBDataRefresh, pFIBDataSet, 
  pFIBErrorHandler, pFIBEventLists, pFIBExports, pFIBFieldsDescr, 
  pFIBInterfaces, pFIBLists, pFIBMetaData, pFIBProps, pFIBQuery, pFIBScripter, 
  pFIBSQLLog, pFIBStoredProc, pTestInfo, RegFIBPlus, RegUtils, SIBAPI, 
  SIBEABase, SIBFIBEA, SIBGlobals, SqlTxtRtns, StdFuncs, StrUtil, VariantRtn, 
  SqlTimSt, DBConsts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pFIBClientDataSet', @pFIBClientDataSet.Register);
  RegisterUnit('RegFIBPlus', @RegFIBPlus.Register);
end;

initialization
  RegisterPackage('FIB', @Register);
end.

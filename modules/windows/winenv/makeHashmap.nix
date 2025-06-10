{lib, ...}: let
  toXmlPropertyVal = val: let
    arrVal =
      if builtins.typeOf val == "string"
      then [val]
      else val;
    finalVal = lib.strings.concatStringsSep ";" arrVal;
  in ''<S N="Value">${lib.strings.escapeXML val}</S>'';
  toXmlKeyVal = key: val: ''
    <En>
      <S N="Key">${lib.strings.escapeXML key}</S>
      ${toXmlPropertyVal val}
    </En>
  '';
  toPsHashmap = vars: let
    properties = lib.mapAttrsToList toXmlKeyVal vars;
    propStr = lib.strings.concatStringsSep "\n      " properties;
  in ''
    <Objs Version="1.1.0.1" xmlns="http://schemas.microsoft.com/powershell/2004/04">
      <Obj RefId="0">
        <TN RefId="0">
          <T>System.Collections.Hashtable</T>
          <T>System.Object</T>
        </TN>
        <DCT>
          ${propStr}
        </DCT>
      </Obj>
    </Objs>
  '';
in
  toPsHashmap

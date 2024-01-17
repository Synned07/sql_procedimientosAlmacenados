Create FUNCTION extraerData (
    @datos VARCHAR(100)
)
RETURNS TABLE
as
 RETURN(       SELECT
            value AS Elemento,
            ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS Indice
        FROM STRING_SPLIT(@datos, ',')
 )
 go
DROP PROCEDURE dbo.insertarDatosAnyTabla;
go
Create PROCEDURE insertarDatosAnyTabla
    @TableName NVARCHAR(50),
    @Values NVARCHAR(MAX)
AS
BEGIN
    DECLARE @SQL NVARCHAR(MAX)=''
    DECLARE @ColumnasFormateadas NVARCHAR(MAX) =''

    SELECT @ColumnasFormateadas = COALESCE(@ColumnasFormateadas + ',','') + COLUMN_NAME
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME = @TableName and 0 = CHARINDEX('id',COLUMN_NAME);
   
    PRINT STUFF(@ColumnasFormateadas, 1, 1, ' ') 

    SELECT @SQL = 'INSERT INTO ' + @TableName + ' (' + STUFF(@ColumnasFormateadas, 1, 1, ' ') + ') VALUES (' + @Values + ')'

    PRINT @SQL
    EXEC sp_executesql @SQL
END

DROP PROCEDURE dbo.editarDatosAnyTabla;
go
Create PROCEDURE editarDatosAnyTabla
    @TableName NVARCHAR(50),
    @Values NVARCHAR(MAX)
AS
BEGIN
    DECLARE @SQL NVARCHAR(MAX)=''
    DECLARE @ColumnasFormateadas NVARCHAR(MAX) =''
    DECLARE @ColumnaId NVARCHAR(50) =''

    SELECT  @ColumnasFormateadas =  @ColumnasFormateadas+', ' + o.COLUMN_NAME+' = ' + v.[Elemento]
    FROM INFORMATION_SCHEMA.COLUMNS o left JOIN (SELECT *  from dbo.extraerData(@Values))  v on o.ORDINAL_POSITION = v.[indice]
    WHERE TABLE_NAME = @TableName and 0 = CHARINDEX('id',COLUMN_NAME) and o.ORDINAL_POSITION != 1 ;

    PRINT STUFF(@ColumnasFormateadas, 1, 1, ' ')
   
    SELECT top 1 @ColumnaId  = COLUMN_NAME +' = ' + CAST(v.Elemento as varchar(10)) 
    from INFORMATION_SCHEMA.COLUMNS, (SELECT *  from dbo.extraerData(@Values))  v  
    WHERE TABLE_NAME = 'perfil' AND v.Indice = 1;

    PRINT STUFF(@ColumnasFormateadas, 1, 1, ' ') 
    PRINT @ColumnaId
    PRINT @IDregistro

    SELECT @SQL = 'UPDATE ' + @TableName + ' SET ' + STUFF(@ColumnasFormateadas, 1, 1, ' ') +' WHERE '+ @ColumnaId
    PRINT @SQL

    EXEC sp_executesql @SQL
END


EXEC dbo.insertarDatosAnyTabla @TableName = 'metodoPago', @Values ='''chao2'',2'

EXEC dbo.editarDatosAnyTabla @TableName = 'metodoPago', @Values ='''chaogil'',1', @IDregistro = 5

select * from metodoPago

-- Update rows in table '[TableName]' in schema '[dbo]'
UPDATE [dbo].[TableName]
SET
    [ColumnName1] = ColumnValue1,
    [ColumnName2] = ColumnValue2
    -- Add more columns and values here
WHERE /* add search conditions here */
GO





go

DECLARE @ColumnasFormateadas NVARCHAR(MAX) =''
DECLARE @cadena NVARCHAR(100) = '3,''dato1'',dato2';
SELECT top 1 @ColumnasFormateadas = COLUMN_NAME +' = ' + CAST(v.Elemento as varchar(10)) from INFORMATION_SCHEMA.COLUMNS, (SELECT *  from dbo.extraerData(@cadena))  v  WHERE TABLE_NAME = 'perfil';
    PRINT @ColumnasFormateadas

    SELECT  @ColumnasFormateadas =  @ColumnasFormateadas+', ' + o.COLUMN_NAME+' = ' + v.[Elemento]
    FROM INFORMATION_SCHEMA.COLUMNS o left JOIN (SELECT *  from dbo.extraerData(@cadena)) as v on o.ORDINAL_POSITION = v.[indice]
    WHERE TABLE_NAME = 'perfil' and 0 = CHARINDEX('id',COLUMN_NAME) and o.ORDINAL_POSITION != 1 ;

    PRINT STUFF(@ColumnasFormateadas, 1, 1, ' ')

go

DECLARE @cadena NVARCHAR(100) = '9,''dato1'',dato2';
SELECT o.ORDINAL_POSITION, v.[indice] , v.Elemento from INFORMATION_SCHEMA.COLUMNS o 
left JOIN (SELECT *  from dbo.extraerData(@cadena)) as v on o.ORDINAL_POSITION = v.[indice] WHERE TABLE_NAME = 'perfil' 



SELECT *  from dbo.extraerData('Uno,Dos,Tres,Cuatro,Cinco')



DECLARE @Cadena VARCHAR(100) = 'Uno,Dos,Tres,Cuatro,Cinco';

WITH SplitCadena AS (
    SELECT
        value AS Elemento,
        ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS Indice
    FROM STRING_SPLIT(@Cadena, ',')
)
SELECT *
FROM SplitCadena;

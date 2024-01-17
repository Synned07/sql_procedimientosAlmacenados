--Procedimiento almacenados Padre para Listar
ALTER PROCEDURE ListarPTabla
	@Tabla NVARCHAR(MAX)
AS
BEGIN
	BEGIN TRY
		DECLARE @NumeroLlaves INT;

		SELECT @NumeroLlaves = COUNT(COLUMN_NAME) 
		FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
		WHERE TABLE_NAME = @Tabla;

		IF(@NumeroLlaves = 1)
		BEGIN 
			EXECUTE('SELECT * FROM '+@Tabla);
		END
		ELSE BEGIN 
			DECLARE @Indice INT = 1;
			DECLARE @Maximo INT = (
				SELECT COUNT(*)
				FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
					INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
						ON K.COLUMN_NAME = C.COLUMN_NAME
				WHERE k.TABLE_NAME = @Tabla AND c.TABLE_NAME = @Tabla
			);

			DECLARE @Consulta NVARCHAR(MAX) = 'SELECT * FROM '+@Tabla;

			WHILE @Indice <= @Maximo BEGIN 
				DECLARE @pk_name NVARCHAR(MAX) = '';
				DECLARE @nombre_columna NVARCHAR(MAX) = '';
				SELECT TOP 1 @pk_name = REPLACE(K.CONSTRAINT_NAME, 'FK_'+@Tabla+'_', ''), @nombre_columna = K.COLUMN_NAME
				FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K 
					INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
						ON K.COLUMN_NAME = C.COLUMN_NAME
				WHERE K.TABLE_NAME = @Tabla AND C.TABLE_NAME = @Tabla AND K.CONSTRAINT_NAME LIKE '%FK%' AND C.ORDINAL_POSITION = @Indice;

				IF @pk_name != '' AND @nombre_columna != '' BEGIN 
					SET @pk_name = REPLACE(@pk_name, '_'+@nombre_columna, '');
					SET @pk_name = 'PK_'+@pk_name;

					DECLARE @flag INT = 0;

					SELECT @Consulta +=
						CASE
							WHEN COUNT(CTU.TABLE_NAME) = 1 THEN 		
								' INNER JOIN ' + CTU.TABLE_NAME + ' ON ' + @Tabla + '.' + @nombre_columna + ' = ' + CTU.TABLE_NAME + '.' + KCU.COLUMN_NAME + ' '
							ELSE 
								SET @flag = 1;
						END
					FROM INFORMATION_SCHEMA.CONSTRAINT_TABLE_USAGE AS CTU 
						INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KCU
							ON CTU.TABLE_NAME = KCU.TABLE_NAME
					WHERE CTU.CONSTRAINT_NAME = @pk_name
					GROUP BY CTU.TABLE_NAME, KCU.COLUMN_NAME;


					IF (@flag = 1) BEGIN
						seguimos alimentando la consulta.
						WHILE @Indice2 <= COUNT(CTU.TABLE_NAME) BEGIN

							SELECT TOP 1 @pk_name = REPLACE(K.CONSTRAINT_NAME, 'FK_'+@CTU_TABLE_NAME+'_', ''), @nombre_columna2 = K.COLUMN_NAME
							FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K 
								INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
									ON K.COLUMN_NAME = C.COLUMN_NAME
							WHERE K.TABLE_NAME = @CTU_TABLE_NAME AND C.TABLE_NAME = @CTU_TABLE_NAME AND K.CONSTRAINT_NAME LIKE '%FK%' AND C.ORDINAL_POSITION = @Indice2;

							IF(@nombre_columna2 != '') BEGIN 
								SET @pk_name = REPLACE(@pk_name, '_'+@nombre_columna2, '');
								SET @pk_name = 'PK_'+@pk_name;
							
								SELECT ' INNER JOIN ' + CTU2.TABLE_NAME + ' ON ' + CTU2.TABLE_NAME + '.' + CTU2.COLUMN_NAME + ' = ' + @CTU_TABLE_NAME + '.' + @CTU_COLUMN_NAME
								FROM INFORMATION_SCHEMA.CONSTRAINT_TABLE_USAGE AS CTU2 
									INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KCU2
										ON CTU2.TABLE_NAME = KCU2.TABLE_NAME
								WHERE CTU2.CONSTRAINT_NAME = @pk_name;

							END
							
							SET @Indice2 += 1
						END
					END 
						
				END
				SET @Indice += 1;
			END

			PRINT @Consulta;
		END

	END TRY
	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END


EXECUTE ListarPTabla @Tabla = 'reserva';

SELECT K.COLUMN_NAME, C.ORDINAL_POSITION, K.CONSTRAINT_NAME
FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
	INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
		ON K.COLUMN_NAME = C.COLUMN_NAME
WHERE k.TABLE_NAME = 'reserva' AND c.TABLE_NAME = 'reserva';


DROP PROCEDURE ListarPTabla;

GO

--Procedimiento almacenados Padre para Crear
ALTER PROCEDURE CrearPTabla
	@Tabla NVARCHAR(90),
	@NumeroCampos INT,
	@Esquema NVARCHAR(MAX),
	@Registros NVARCHAR(MAX)
AS
BEGIN
	BEGIN TRY
		DECLARE @SQL NVARCHAR(MAX) = N'INSERT INTO ' + @Tabla + '(' + @Esquema + ')' + ' VALUES(';
		DECLARE @VALUES NVARCHAR(MAX) = dbo.FuncionString(@NumeroCampos, @Esquema, @Registros, 'crear'); 
		SET @SQL += @VALUES + ')';

		EXECUTE( @SQL );
		SELECT 'ok';
	
		-- PRINT @SQL;
	END TRY
	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END


EXECUTE CrearPTabla 
				@Tabla = 'reserva', 
				@NumeroCampos = 4, 
				@Esquema = 'modeloAutoId, calendarioid, pruebaManejoId, usuarioPerfil_id',  
				@Registros = '10, 59, 46, 35';

EXECUTE ListarPTabla @Columnas = '*',
						@Tabla = 'calendario';


DROP PROCEDURE CrearPTabla;
GO
--Procedimiento almacenados Padre para Eliminar
ALTER PROCEDURE EliminarPTabla
	@Tabla NVARCHAR(80) = '',
	@key NVARCHAR(80) = '',
	@Id INT = 0
AS
BEGIN

	BEGIN TRY
		IF (NOT @Tabla = '') AND (NOT @key = '') AND (NOT @Id = 0) BEGIN
			DECLARE @CMD NVARCHAR(MAX) = 'DELETE FROM';
			SET @CMD += ' ' + @Tabla;
			SET @CMD += ' ' + 'WHERE';
			SET @CMD += ' ' + @key;
			SET @CMD += ' = ' + CAST(@Id AS NVARCHAR(MAX));

			EXECUTE(@CMD);

			SELECT 'ok';
		END
		ELSE BEGIN 
			SELECT 'error';
		END
	END TRY

	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END

EXEC EliminarPTabla 
				@Tabla = 'usuario', 
				@Key = 'usuario_id', 
				@Id = 44;


INSERT INTO reserva(modeloAutoId, calendarioId, pruebaManejoId, usuarioPerfil_id)
	VALUES(10, 3, 4, 11);

GO

--Procedimiento almacenados Padre para Actualizar
ALTER PROCEDURE ActualizarPTabla
	@Tabla NVARCHAR(MAX),
	@NRelaciones INT,
	@Campos NVARCHAR(MAX),
	@Valor NVARCHAR(MAX),
	@NCondicional INT = 0, 
	@CampoCondicional NVARCHAR(MAX) = '',
	@ValorCondicional NVARCHAR(MAX) = ''
AS
BEGIN 
	BEGIN TRY
		DECLARE @CMD NVARCHAR(MAX);
		SET @CMD = N'UPDATE ' + @Tabla + ' SET ';

		DECLARE @Relacion NVARCHAR(MAX) = dbo.FuncionString(@NRelaciones, @campos, @valor, 'actualizar');
		SET @CMD += @Relacion;

		IF @CampoCondicional != '' AND @ValorCondicional != '' AND @NCondicional != 0 BEGIN 
			SET @CMD += ' WHERE ';
			DECLARE @Condicional NVARCHAR(MAX) = dbo.FuncionString( @NCondicional, @CampoCondicional, @ValorCondicional, 'actualizar2');
			SET @CMD += @Condicional;
		END


		EXEC sp_sqlexec @CMD;
		SELECT 'ok';
	END TRY
	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END


EXEC ActualizarPTabla 
					@Tabla = 'metodoPago', 
					@NRelaciones = 2, 
					@Campos = 'Metodopago_tipo, MetodoPago_estado', 
					@Valor = 'trasferencia, habilitado', 
					@NCondicional = 1, 
					@CampoCondicional = 'Metodopago_id', 
					@ValorCondicional = '1';	



GO
/* =================================================== */
--			     FUNCION SUBSTRING                      --
/* =================================================== */


GO

-- funcion con valores de tabla
ALTER FUNCTION FuncionString(@NColumnas INT, @Columnas NVARCHAR(MAX), @Columnas2 NVARCHAR(MAX), @Tipo NVARCHAR(50)) 
	RETURNS NVARCHAR(MAX)
AS
BEGIN
	DECLARE @Indice INT = 1
	DECLARE @NColumna1 NVARCHAR(MAX)
	DECLARE @NColumna2 NVARCHAR(MAX)
	DECLARE @caracter NVARCHAR(2);

	DECLARE @CMD NVARCHAR(MAX) = '';
	WHILE @Indice <= @NColumnas BEGIN
		/* Sacamos nuestra cadena  */
		IF @Indice != @NColumnas BEGIN
			-- IF @Indice = 1 BEGIN
			-- 	SET @caracter = SUBSTRING(@Columnas2, 1 , 1);

			-- 	STUFF(@Columnas2, 1,  );
			-- END

			SET @NColumna1 = SUBSTRING(@Columnas, 0, CHARINDEX(',', @Columnas, 0))
			SET @NColumna2 = SUBSTRING(@Columnas2, 0, CHARINDEX(',', @Columnas2, 0))
			
			/*  lo reemplzamos en nuestra cadena principal */
			SET @Columnas = REPLACE(@Columnas, @NColumna1+',', '')
			SET @Columnas2 = TRIM(STUFF(@Columnas2, 1, LEN(@NColumna2+','), ''))

		END
		ELSE BEGIN
			SET @NColumna1 = TRIM(@Columnas);
			SET @NColumna2 = TRIM(@Columnas2);
		END
		
		IF LOWER(@Tipo) = 'listar' BEGIN 
			SET @CMD += ' INNER JOIN ' + @NColumna1 + ' ON ' + @NColumna2
		END
		
		IF LOWER(@Tipo) = 'crear' BEGIN
			SET @CMD += '''' + TRIM(@NColumna2) + '''';
			IF @Indice != @NColumnas BEGIN
				SET @CMD += ',';
			END
		END

		IF LOWER(@Tipo) = 'actualizar' BEGIN 
			SET @CMD += @NColumna1 + ' = ';
			SET @CMD += '''' + TRIM(@NColumna2) + '''';

			IF @Indice != @NColumnas BEGIN 
				SET @CMD += ', ';
			END
		END

		IF LOWER(@Tipo) = 'actualizar2' BEGIN 
			SET @CMD += @NColumna1 + ' = ';
			SET @CMD += '''' + TRIM(@NColumna2) + '''';

			IF @Indice != @NColumnas BEGIN 
				SET @CMD += ' AND ';
			END
		END
		
		SET @Indice += 1
	END
	
	RETURN @CMD;
END

GO


DECLARE @result NVARCHAR(80) = dbo.FuncionString(3,'relacion1, relacion, relacion2', '(relacion1), (relacion2), (relaciones3)', 'crear');
PRINT @result;

GO

SELECT * FROM UsuarioPerfil;
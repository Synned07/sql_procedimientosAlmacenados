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
			DECLARE @Consulta NVARCHAR(MAX) = 'SELECT * FROM '+@Tabla+' AS tabla_1 ';

			SELECT @Consulta += ' INNER JOIN ' +
				(CASE
					WHEN @Tabla = 'compra' AND TC.CONSTRAINT_NAME LIKE '%metodoPagoId' THEN 'metodoPago AS tabla_2'
					WHEN @Tabla = 'compra' AND TC.CONSTRAINT_NAME LIKE '%usuarioPerfil_id' THEN 'usuarioPerfil AS tabla_3' 

					WHEN @Tabla = 'reserva' AND TC.CONSTRAINT_NAME LIKE '%calendarioId' THEN 'calendario AS tabla_2'
					WHEN @Tabla = 'reserva' AND TC.CONSTRAINT_NAME LIKE '%modeloAutoId' THEN 'modeloAuto AS tabla_3'
					WHEN @Tabla = 'reserva' AND TC.CONSTRAINT_NAME LIKE '%pruebaManejoId' THEN 'pruebaManejo AS tabla_4'
					WHEN @Tabla = 'reserva' AND TC.CONSTRAINT_NAME LIKE '%usuarioPerfil_id' THEN 'UsuarioPerfil AS tabla_5'
				END) + ' ON ' + 'tabla_1.' + KCU.COLUMN_NAME + ' = ' + (
					CASE 
						WHEN @Tabla = 'compra' AND TC.CONSTRAINT_NAME LIKE '%metodoPagoId' THEN 'tabla_2.Metodopago_id'
						WHEN @Tabla = 'compra' AND TC.CONSTRAINT_NAME LIKE '%usuarioPerfilId' THEN 'tabla_3.usuarioPerfil_id'

						WHEN @Tabla = 'reserva' AND TC.CONSTRAINT_NAME LIKE '%calendarioId' THEN 'tabla_2.calendario_id'
						WHEN @Tabla = 'reserva' AND TC.CONSTRAINT_NAME LIKE '%modeloAutoId' THEN 'tabla_3.modeloAuto_id'
						WHEN @Tabla = 'reserva' AND TC.CONSTRAINT_NAME LIKE '%pruebaManejoId' THEN 'tabla_4.pruebaManejo_id'
						WHEN @Tabla = 'reserva' AND TC.CONSTRAINT_NAME LIKE '%usuarioPerfil_id' THEN 'tabla_5.usuarioPerfil_id'
					END
				)
			FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS TC
				INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KCU
					ON TC.CONSTRAINT_NAME = KCU.CONSTRAINT_NAME
			WHERE TC.TABLE_NAME = @Tabla AND TC.CONSTRAINT_TYPE = 'FOREIGN KEY';
		
			EXECUTE(@Consulta);
		END
	END TRY
	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END




EXECUTE ListarPTabla @Tabla = 'reserva';

GO

SELECT * FROM INFORMATION_SCHEMA.COLUMN_DOMAIN_USAGE;
SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE;

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
--Procedimiento almacenados Padre para Listar
ALTER PROCEDURE ListarPTabla
	@Tabla NVARCHAR(MAX),
	@campo NVARCHAR(MAX) = '',
	@valor NVARCHAR(MAX) = ''
AS
BEGIN
	BEGIN TRY
		SET @Tabla = LOWER(@Tabla);
		DECLARE @NumeroLlaves INT;
		DECLARE @Columnas NVARCHAR(MAX);

		SELECT @NumeroLlaves = COUNT(COLUMN_NAME) 
		FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
		WHERE TABLE_NAME = @Tabla;

		

		IF @NumeroLlaves = 1
		BEGIN 
			EXECUTE('SELECT * FROM '+@Tabla);
		END
		ELSE IF @NumeroLlaves > 1
		BEGIN
			IF(LOWER(@Tabla) = 'reserva') BEGIN SET @Columnas = 'reserva_id, calendario_asunto, Fecha, Hora, nombre, apellido, cedula, celular, correo, nombre_rol, estado, Color, Marca, YearAntiguedad, pruebaManejo_descripcion, pruebaManejo_estado, pruebaManejo_nivelSatisfaccion';  END 
			IF(LOWER(@Tabla) = 'usuarioperfil') BEGIN SET @Columnas = 'nombre, apellido, cedula, celular, correo, contrasena, nombre_rol, estado';  END 

			DECLARE @Consulta NVARCHAR(MAX) = 'SELECT ' + @Columnas + ' FROM '+@Tabla;

			DECLARE @EXIT INT = 1;

			WHILE @EXIT = 1 BEGIN 
			
				DECLARE @Indice INT = 1;
				DECLARE @Maximo INT = (
					SELECT COUNT(*)
					FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
						INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
							ON K.COLUMN_NAME = C.COLUMN_NAME
					WHERE k.TABLE_NAME = @Tabla AND c.TABLE_NAME = @Tabla
				);

				DECLARE @verificarTabla NVARCHAR(MAX);

				WHILE @Indice <= @Maximo 
				BEGIN 
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

						
						SELECT @Consulta += ' INNER JOIN ' + CTU.TABLE_NAME + ' ON ' + @Tabla + '.' + @nombre_columna + ' = ' + CTU.TABLE_NAME + '.' + KCU.COLUMN_NAME + ' ', 
								@verificarTabla = (
									SELECT 
										CASE
											WHEN COUNT(x.TABLE_NAME) != 1 THEN x.TABLE_NAME
											ELSE 
												'ninguno'
										END
									FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS x
									WHERE x.TABLE_NAME = CTU.TABLE_NAME
									GROUP BY x.TABLE_NAME
								) 
						FROM INFORMATION_SCHEMA.CONSTRAINT_TABLE_USAGE AS CTU 
							INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KCU
								ON CTU.TABLE_NAME = KCU.TABLE_NAME
						WHERE CTU.CONSTRAINT_NAME = @pk_name AND KCU.CONSTRAINT_NAME LIKE '%PK%';

					END

					SET @Indice += 1  
				END			

				IF (@Indice-1) = @Maximo AND @verificarTabla != 'ninguno' BEGIN 
					SET @Tabla = @verificarTabla;
				END
				ELSE BEGIN 
					SET @EXIT = 0;
				END
			END

			--clausula (filtrador)
			IF @campo != '' AND @valor != '' BEGIN 
				SET @Consulta += ' WHERE ' + @campo + ' = ' + @valor;
			END 

			EXECUTE(@Consulta);

		END
		ELSE BEGIN 
			SELECT 'error';
		END
	END TRY
	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END

EXECUTE ListarPTabla @Tabla = 'reserva';

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


DECLARE @result NVARCHAR(80) = dbo.FuncionString(3,'relacion1, relacion, relacion2', '(fidjidf), (difjid), (dfiji)', 'crear');
PRINT @result;

GO

SELECT * FROM UsuarioPerfil;
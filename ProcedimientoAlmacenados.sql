--Procedimiento almacenados Padre para Listar
ALTER PROCEDURE ListarPTabla
	@Tabla NVARCHAR(MAX),
	@nRelaciones INT = 0,
	@campo NVARCHAR(MAX) = '',
	@valor NVARCHAR(MAX) = '',
	@resultadoSalida NVARCHAR(MAX) = '' OUTPUT
AS
BEGIN
	BEGIN TRY
		SET @Tabla = LOWER(@Tabla);
		DECLARE @NumeroLlaves INT;
		DECLARE @Columnas NVARCHAR(MAX);

		DECLARE @comprobacion BIT = 1;
		DECLARE @VERIFICAR BIT = 1;
		CREATE TABLE #resultado( resultado_id INT );

		-- con este verifico cuantas claves FK tiene una tabla en general
		SELECT @NumeroLlaves = COUNT(COLUMN_NAME) 
		FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
		WHERE TABLE_NAME = @Tabla;

		IF @NumeroLlaves = 1
		BEGIN 

			DECLARE @sql NVARCHAR(MAX) = 'SELECT COUNT(*) ';

			WHILE @VERIFICAR = 1 BEGIN 
				SET @sql += 'FROM ' + @Tabla;
				
				IF @campo != '' AND @valor != '' 
				BEGIN 
					SET @sql += ' WHERE ' + dbo.FuncionString(@nRelaciones, @campo, @valor, 'verificar');
				END

				IF @comprobacion = 1 BEGIN 
					INSERT INTO #resultado EXECUTE(@sql)

					IF (SELECT resultado_id FROM #resultado) = 0 BEGIN 
						SELECT 'sin_data';
						
						SET @resultadoSalida = 'sin_data';
						SET @VERIFICAR = 0;
					END
					ELSE BEGIN 
						SET @sql = 'SELECT *';
					END

					SET @comprobacion = 0;
					DROP TABLE #resultado;
				END
				ELSE BEGIN 
					EXECUTE(@sql);
					BREAK;
				END
			END

		END
		ELSE IF @NumeroLlaves > 1
		BEGIN
			BEGIN TRY 
				
				DECLARE @Consulta NVARCHAR(MAX) = 'SELECT ' + 'COUNT(*)';
				DECLARE @TablaOriginal NVARCHAR(MAX) = @Tabla;

				-- comprobacion antes de realizar la construccion de la consulta.
				IF(@Tabla = 'reserva') BEGIN SET @Columnas = 'reserva_id, calendario_asunto, Fecha, Hora, nombre, apellido, cedula, celular, correo, nombre_rol, estado, Color, Marca, YearAntiguedad, pruebaManejo_descripcion, pruebaManejo_estado, pruebaManejo_nivelSatisfaccion';  END 
				IF(@Tabla = 'usuarioperfil') BEGIN SET @Columnas = 'nombre, apellido, cedula, celular, correo, contrasena, nombre_rol, estado';  END 
				IF(@Tabla = 'compra') BEGIN SET @Columnas = '*';  END
				
				WHILE @VERIFICAR = 1
				BEGIN 

					IF @comprobacion = 0 BEGIN 
						SET @Tabla = @TablaOriginal;
					END

					SET @Consulta += ' FROM '+@Tabla;

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
						SET @Consulta += ' WHERE ' + dbo.FuncionString(@nRelaciones, @campo, @valor, 'verificar');
					END 

					--tabla temporal...
					IF @comprobacion = 1 BEGIN 
					
						INSERT INTO #resultado EXECUTE(@Consulta)

						IF (SELECT resultado_id FROM #resultado) = 0 BEGIN 
							SELECT 'sin_data' as data_error;
							
							SET @resultadoSalida = 'sin_data';
							SET @VERIFICAR = 0;
						END
						ELSE BEGIN 
							SET @Consulta = 'SELECT ' + @Columnas;
						END

						SET @comprobacion = 0;
						DROP TABLE #resultado;

					END 
					ELSE BEGIN
						EXECUTE(@Consulta); 
						SET @VERIFICAR = 0;
					END

				END
			END TRY 
			BEGIN CATCH 
				SELECT 'error' as error;
				SET @resultadoSalida = 'error';
			END CATCH 

		END
		ELSE BEGIN 
			SELECT 'error' as error;
			SET @resultadoSalida = 'error';
		END
	END TRY
	BEGIN CATCH
		-- THROW 50001, 'error', 1;
		SELECT 'error' as error;
		SET @resultadoSalida = 'error';
	END CATCH
END


EXECUTE ListarPTabla @Tabla = 'usuario', @campo='cedula', @valor='8888888887';


GO

--Procedimiento almacenados Padre para Crear
ALTER PROCEDURE CrearPTabla
	@TablaEntrada NVARCHAR(90),
	@NumeroCampos INT,
	@Esquema NVARCHAR(MAX),
	@Registros NVARCHAR(MAX)
AS
BEGIN
	BEGIN TRY
		
		DECLARE @resultadoTexto NVARCHAR(MAX) = '';
		EXECUTE ListarPTabla @Tabla = @TablaEntrada, @NRelaciones = @NumeroCampos, @campo=@Esquema, @valor=@Registros, @resultadoSalida = @resultadoTexto OUTPUT;

		IF @resultadoTexto != 'sin_data' AND @resultadoTexto != 'error' BEGIN 
			DECLARE @SQL NVARCHAR(MAX) = N'INSERT INTO ' + @TablaEntrada + '(' + @Esquema + ')' + ' VALUES(';
			DECLARE @VALUES NVARCHAR(MAX) = dbo.FuncionString(@NumeroCampos, @Esquema, @Registros, 'crear'); 
			SET @SQL += @VALUES + ')';

			EXECUTE( @SQL );
			SELECT 'ok';
		END
		ELSE BEGIN 
			SELECT 'registro_encontrado';
		END

	END TRY
	BEGIN CATCH
		SELECT ERROR_MESSAGE();
	END CATCH
END


EXECUTE CrearPTabla 
				@TablaEntrada = 'reserva', 
				@NumeroCampos = 4, 
				@Esquema = 'modeloAutoId, calendarioid, pruebaManejoId, usuarioPerfil_id',  
				@Registros = '10, 59, 46, 35';


GO



--Procedimiento almacenados Padre para Eliminar
ALTER PROCEDURE EliminarPTabla
	@TablaEntrada NVARCHAR(MAX) = '',
	@key NVARCHAR(MAX) = '',
	@value NVARCHAR(MAX) = ''
AS
BEGIN
	BEGIN TRY 
		DECLARE @resultadoTexto NVARCHAR(MAX) = '';
		EXECUTE ListarPTabla @Tabla = @TablaEntrada, @nRelaciones = 1, @campo=@key, @valor=@value, @resultadoSalida = @resultadoTexto OUTPUT;

		IF (NOT @TablaEntrada = '') AND (NOT @key = '') AND (NOT @value = '') AND @resultadoTexto != 'sin_data' AND @resultadoTexto != 'error' BEGIN
			DECLARE @CMD NVARCHAR(MAX) = 'DELETE FROM';
			SET @CMD += ' ' + @TablaEntrada;
			SET @CMD += ' ' + 'WHERE';
			SET @CMD += ' ' + @key;
			SET @CMD += ' = ' + @value;

			EXECUTE(@CMD);
			SELECT 'ok';
		END
		ELSE BEGIN 
			SELECT 'error';
		END
	END TRY 
	BEGIN CATCH 
		SELECT 'error';
	END CATCH 
END

EXEC EliminarPTabla 
				@TablaEntrada = 'usuario', 
				@Key = 'cedula', 
				@value = '8888888888';



GO

--Procedimiento almacenados Padre para Actualizar
ALTER PROCEDURE ActualizarPTabla
	@TablaEntrada NVARCHAR(MAX),
	@NRelaciones INT,
	@Campos NVARCHAR(MAX),
	@Valor NVARCHAR(MAX),
	@NCondicional INT = 0, 
	@CampoCondicional NVARCHAR(MAX) = '',
	@ValorCondicional NVARCHAR(MAX) = ''
AS
BEGIN 
	BEGIN TRY
		DECLARE @resultadoTexto NVARCHAR(MAX) = '';
		EXECUTE ListarPTabla @Tabla = @TablaEntrada, @nRelaciones = @NCondicional, @campo=@CampoCondicional, @valor=@ValorCondicional, @resultadoSalida = @resultadoTexto OUTPUT;

		IF @resultadoTexto != 'sin_data' AND @resultadoTexto != 'error' BEGIN 

			DECLARE @CMD NVARCHAR(MAX);
			SET @CMD = N'UPDATE ' + @TablaEntrada + ' SET ';

			DECLARE @Relacion NVARCHAR(MAX) = dbo.FuncionString(@NRelaciones, @campos, @valor, 'actualizar');
			SET @CMD += @Relacion;

			IF @CampoCondicional != '' AND @ValorCondicional != '' AND @NCondicional != 0 BEGIN 
				SET @CMD += ' WHERE ';
				DECLARE @Condicional NVARCHAR(MAX) = dbo.FuncionString( @NCondicional, @CampoCondicional, @ValorCondicional, 'actualizar2');
				SET @CMD += @Condicional;
			END


			EXEC sp_sqlexec @CMD;
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


EXEC ActualizarPTabla 
			@TablaEntrada = 'metodoPago', 
			@NRelaciones = 2, 
			@Campos = 'Metodopago_tipo, MetodoPago_estado', 
			@Valor = 'trasferencia, habilitado', 
			@NCondicional = 1, 
			@CampoCondicional = 'Metodopago_id', 
			@ValorCondicional = '8';	


GO
/* =================================================== */
--			     FUNCION SUBSTRING                      --
/* =================================================== */


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

		IF LOWER(@Tipo) = 'verificar' BEGIN 
			SET @CMD += @NColumna1 + ' = ' + @NColumna2;

			IF @Indice != @NColumnas BEGIN 
				SET @CMD += ' OR ';
			END
		END
		
		SET @Indice += 1
	END
	
	RETURN @CMD;
END

GO

DECLARE @result NVARCHAR(80) = dbo.FuncionString(2,'relacion1, relacion2', 'fidjidf, difji', 'verificar');
PRINT @result;

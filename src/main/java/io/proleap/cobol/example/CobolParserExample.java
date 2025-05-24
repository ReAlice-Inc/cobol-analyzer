package io.proleap.cobol.example;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.Trees;

import io.proleap.cobol.CobolLexer;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.CobolParser.StartRuleContext;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.data.workingstorage.WorkingStorageSection;
import io.proleap.cobol.asg.metamodel.identification.IdentificationDivision;
import io.proleap.cobol.asg.metamodel.identification.ProgramIdParagraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.impl.CobolPreprocessorImpl;

/**
 * COBOLパーサーの使用例
 */
public class CobolParserExample {

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("使用方法: java CobolParserExample <cobol-file>");
            System.exit(1);
        }

        String fileName = args[0];
        File cobolFile = new File(fileName);

        if (!cobolFile.exists()) {
            System.err.println("ファイルが見つかりません: " + fileName);
            System.exit(1);
        }

        try {
            // COBOLファイルを解析
            System.out.println("COBOLファイルを解析中: " + fileName);
            Program program = parseCobolFile(cobolFile);

            // 解析結果を表示
            displayParseResults(program);
            
            // ASTツリーファイルを生成
            generateASTTreeFile(cobolFile);

        } catch (Exception e) {
            System.err.println("解析エラー: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * COBOLファイルを解析してProgramオブジェクトを返す
     */
    private static Program parseCobolFile(File cobolFile) throws IOException {
        CobolParserRunnerImpl parser = new CobolParserRunnerImpl();
        
        // FIXED形式でファイルを解析
        return parser.analyzeFile(cobolFile, CobolSourceFormatEnum.FIXED);
    }

    /**
     * ASTツリーファイルを生成
     */
    private static void generateASTTreeFile(File cobolFile) throws IOException {
        System.out.println("\nASTツリーファイルを生成中...");
        
        // 出力ファイル名を決定
        String outputFileName = cobolFile.getName() + ".tree";
        File outputFile = new File(outputFileName);
        
        // プリプロセッサでCOBOLファイルを処理
        CobolParserParams params = new CobolParserParamsImpl();
        params.setCopyBookDirectories(Arrays.asList(cobolFile.getParentFile()));
        params.setFormat(CobolSourceFormatEnum.FIXED);
        
        String preProcessedInput = new CobolPreprocessorImpl().process(cobolFile, params);
        
        // ANTLR4でAST生成
        CobolLexer lexer = new CobolLexer(CharStreams.fromString(preProcessedInput));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CobolParser parser = new CobolParser(tokens);
        StartRuleContext startRule = parser.startRule();
        
        // ツリーを文字列に変換（ANTLR4標準機能を使用）
        String treeString = Trees.toStringTree(startRule, parser);
        
        // ファイルに出力
        try (PrintWriter writer = new PrintWriter(new FileWriter(outputFile))) {
            writer.write(treeString);
        }
        
        System.out.println("ASTツリーファイルが生成されました: " + outputFile.getAbsolutePath());
    }

    /**
     * 解析結果を表示
     */
    private static void displayParseResults(Program program) {
        System.out.println("\n=== ASG解析結果 ===");
        
        // コンパイル単位の情報を表示
        System.out.println("コンパイル単位数: " + program.getCompilationUnits().size());
        
        for (CompilationUnit compilationUnit : program.getCompilationUnits()) {
            System.out.println("\nコンパイル単位名: " + compilationUnit.getName());
            
            ProgramUnit programUnit = compilationUnit.getProgramUnit();
            if (programUnit != null) {
                displayProgramUnitInfo(programUnit);
            }
        }
    }

    /**
     * プログラム単位の情報を表示
     */
    private static void displayProgramUnitInfo(ProgramUnit programUnit) {
        System.out.println("  プログラム単位が見つかりました");
        
        // IDENTIFICATION DIVISION
        IdentificationDivision identificationDivision = programUnit.getIdentificationDivision();
        if (identificationDivision != null) {
            System.out.println("  IDENTIFICATION DIVISION: あり");
            
            ProgramIdParagraph programIdParagraph = identificationDivision.getProgramIdParagraph();
            if (programIdParagraph != null) {
                System.out.println("    プログラム名: " + programIdParagraph.getName());
            }
        }
        
        // DATA DIVISION
        DataDivision dataDivision = programUnit.getDataDivision();
        if (dataDivision != null) {
            System.out.println("  DATA DIVISION: あり");
            
            WorkingStorageSection workingStorageSection = dataDivision.getWorkingStorageSection();
            if (workingStorageSection != null) {
                System.out.println("    WORKING-STORAGE SECTION: あり");
                System.out.println("    データ項目数: " + workingStorageSection.getDataDescriptionEntries().size());
            }
        }
        
        // PROCEDURE DIVISION
        ProcedureDivision procedureDivision = programUnit.getProcedureDivision();
        if (procedureDivision != null) {
            System.out.println("  PROCEDURE DIVISION: あり");
            System.out.println("    パラグラフ数: " + procedureDivision.getParagraphs().size());
        }
    }
} 
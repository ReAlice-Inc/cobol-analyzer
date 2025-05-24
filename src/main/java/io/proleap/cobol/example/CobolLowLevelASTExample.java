package io.proleap.cobol.example;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.Trees;

import io.proleap.cobol.CobolLexer;
import io.proleap.cobol.CobolParser;
import io.proleap.cobol.CobolParser.StartRuleContext;

/**
 * COBOLの低レベルAST（ANTLR4構文解析木）を作成・表示するサンプル
 */
public class CobolLowLevelASTExample {

    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("使用方法: java CobolLowLevelASTExample <COBOLファイルパス>");
            System.exit(1);
        }

        String cobolFilePath = args[0];
        
        try {
            // COBOLファイルを読み込み
            String cobolSource = readCobolFile(cobolFilePath);
            System.out.println("COBOLファイルを読み込み中: " + cobolFilePath);
            
            // 低レベルASTを作成
            StartRuleContext ast = createLowLevelAST(cobolSource);
            System.out.println("低レベルAST作成完了");
            
            // ASTの情報を表示
            displayASTInfo(ast);
            
            // ASTを文字列として出力
            String astString = getASTAsString(ast);
            System.out.println("\n=== AST構文解析木 ===");
            System.out.println(astString);
            
            // ASTをファイルに保存
            saveASTToFile(astString, cobolFilePath + ".ast");
            
        } catch (Exception e) {
            System.err.println("エラーが発生しました: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * COBOLファイルを読み込む
     */
    private static String readCobolFile(String filePath) throws IOException {
        return new String(Files.readAllBytes(Paths.get(filePath)));
    }

    /**
     * 低レベルAST（ANTLR4構文解析木）を作成
     */
    public static StartRuleContext createLowLevelAST(String cobolSource) {
        // 1. レキサー（字句解析器）を作成
        CobolLexer lexer = new CobolLexer(CharStreams.fromString(cobolSource));
        
        // 2. トークンストリームを作成
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        
        // 3. パーサー（構文解析器）を作成
        CobolParser parser = new CobolParser(tokens);
        
        // 4. 構文解析を実行してASTを取得
        StartRuleContext ast = parser.startRule();
        
        return ast;
    }

    /**
     * ASTの基本情報を表示
     */
    private static void displayASTInfo(StartRuleContext ast) {
        System.out.println("\n=== AST基本情報 ===");
        System.out.println("ルートノード: " + ast.getClass().getSimpleName());
        System.out.println("子ノード数: " + ast.getChildCount());
        System.out.println("テキスト長: " + ast.getText().length());
        
        // 子ノードの情報を表示
        System.out.println("\n=== 子ノード情報 ===");
        for (int i = 0; i < ast.getChildCount(); i++) {
            ParseTree child = ast.getChild(i);
            System.out.println("子ノード[" + i + "]: " + child.getClass().getSimpleName());
        }
    }

    /**
     * ASTを文字列として取得
     */
    public static String getASTAsString(StartRuleContext ast) {
        // パーサーを再作成（文字列表現のため）
        CobolLexer lexer = new CobolLexer(CharStreams.fromString(""));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CobolParser parser = new CobolParser(tokens);
        
        return Trees.toStringTree(ast, parser);
    }

    /**
     * ASTをファイルに保存
     */
    private static void saveASTToFile(String astString, String outputPath) {
        try {
            Files.write(Paths.get(outputPath), astString.getBytes());
            System.out.println("ASTファイルを保存しました: " + outputPath);
        } catch (IOException e) {
            System.err.println("ASTファイルの保存に失敗しました: " + e.getMessage());
        }
    }
} 